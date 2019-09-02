/**
 * SL4: A small, bad Lisp
 * Copyright 2019 Jack Conger
 */

/**
 * TODO: Finish migrating to entirely continuation-based
 * TODO: Add continuations
 * TODO: Implement GC
 * TODO: Figure out how best to do macros
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define error(msg...) (fprintf(stderr, msg), exit(1))

/**
 * The core data types: S-expression and friends.
 * A tagged union of either an atom or a cons pair.
 */

enum S_exp_type { SYM, CONS, PRIM, PROC };
typedef struct S_exp {
    enum S_exp_type type;
    union {
        char *str;
        struct S_exp *pair[2];
        /* exp, env, cont, result */
        struct S_exp *(*op)(struct S_exp *, struct S_exp *,
                            struct S_exp *, struct S_exp *);
    } u;
} S_exp;

void write_S(FILE *f, S_exp *exp);


/**
 * Globals.
 */

static S_exp *nil, *tee, *quote, *noeval, *rest, *a_result;


/**
 * Some core data functions.
 */

#define IS_ATOM(exp)      ((exp)->type == SYM || (exp)->type == PRIM)
#define IS_EQ(exp1, exp2) ((exp1)->type == SYM && \
                           (exp2)->type == SYM && \
                           !strcmp((exp1)->u.str, (exp2)->u.str))
#define IS_NIL(exp)       (IS_EQ((exp), nil))

#define car(exp) (IS_ATOM(exp) ? (exp) : (exp)->u.pair[0])
#define cdr(exp) (IS_ATOM(exp) ?  nil  : (exp)->u.pair[1])

S_exp *cons(S_exp *car, S_exp *cdr) {
    S_exp *s = malloc(sizeof(S_exp));
    s->type = CONS;
    s->u.pair[0] = car;
    s->u.pair[1] = cdr;
    return s;
}

S_exp *atom(char *nm) {
    S_exp *s = malloc(sizeof(S_exp));
    s->type = SYM;
    s->u.str = nm;
    return s;
}

S_exp *assoc(S_exp *key, S_exp *env) {
    return (IS_NIL(env)                 ? nil
            : IS_EQ(car(car(env)), key) ? car(env)
            : assoc(key, cdr(env)));
}

S_exp *append(S_exp *l, S_exp *r) {
    return (IS_NIL(l) ? r
            : cons(car(l), append(cdr(l), r)));
}

S_exp *pair(S_exp *cars, S_exp *cdrs) {
    return (IS_NIL(cars) ? nil
            : cons(cons(car(cars), car(cdrs)),
                   pair(cdr(cars), cdr(cdrs))));
}


/**
 * Reading and writing.
 */

#define DEFAULTMAXBUFLEN 20

static char *tokbuf;
static int maxbuflen;
static int buflen;

void bufpush(int ch) {
    if (buflen >= maxbuflen - 1) {
        maxbuflen *= 2;
        tokbuf = realloc(tokbuf, maxbuflen);
    }
    tokbuf[buflen++] = ch;
    tokbuf[buflen] = '\0';
}

static int isparked = 0;
static char *parkedtoken = NULL;

void parktoken(char *token) {
    isparked = 1;
    parkedtoken = token;
}

char *readtoken(FILE *f) {
    int ch;

    if (isparked) {
        isparked = 0;
        return parkedtoken;
    }

    buflen = 0;
    maxbuflen = DEFAULTMAXBUFLEN;
    tokbuf = malloc(sizeof(char) * maxbuflen);

again:
    do {
        if ((ch = getc(f)) == EOF)
            error("EOF\n");
    } while (isspace(ch));

    if (ch == '#') {
        while ((ch = getc(f)) != '\n')
            if (ch == EOF)
                error("EOF\n");
        goto again;
    }

    bufpush(ch);
    if (strchr("(.)\'", ch))
        return tokbuf;

    for (;;) {
        if ((ch = getc(f)) == EOF)
            exit(118);

        if (strchr("(.)\'", ch) || isspace(ch)) {
            ungetc(ch, f);
            return tokbuf;
        }
        bufpush(ch);
    }
}

S_exp *readlist(FILE *);

S_exp *read_S(FILE *f) {
    char *token = readtoken(f);
    return (!strcmp(token, "(")    ? readlist(f)
            : !strcmp(token, "\'") ? cons(quote, cons(read_S(f), nil))
            : atom(token));
}

S_exp *readlist(FILE *f) {
    char *token;
    S_exp *exp;

    token = readtoken(f);

    /* this happens if we read '(' and now are reading ')' */
    if (!strcmp(token, ")"))
        return nil;

    /* this happens if we read '( x' and now are reading '. y )' */
    if (!strcmp(token, ".")) {
        exp = read_S(f);
        if (strcmp((token = readtoken(f)), ")"))
            error("Expected ')'; got '%s'\n", token);
        return exp;
    }

    /**
     * this happens if we're reading anything else -- put the token back,
     * read the next list, and do a cons!
     */
    parktoken(token);
    exp = read_S(f);

    return cons(exp, readlist(f));
}

void write_S(FILE *f, S_exp *exp) {
    switch (exp->type) {
    case SYM:
        fprintf(f, "%s", exp->u.str);
        break;
    case CONS:
        fprintf(f, "(");
        for (;;) {
            write_S(f, car(exp));
            if (IS_NIL(cdr(exp))) {
                fprintf(f, ")");
                break;
            }
            exp = cdr(exp);
            if (exp->type != CONS) {
                fprintf(f, " . ");
                write_S(f, exp);
                fprintf(f, ")");
                break;
            }
            fprintf(f, " ");
        }
        break;
    case PRIM:
        fprintf(f, "#<PRIM>");
        break;
    case PROC:
        fprintf(f, "(lambda ");
        write_S(f, car(exp));
        fprintf(f, " ");
        write_S(f, car(cdr(exp)));
        fprintf(f, ")");
        break;
    default:
        error("S-expression of unexpected type %d\n", exp->type);
    }
}


/**
 * Evaluation.
 */

S_exp *eval(S_exp *exp, S_exp *env);
S_exp *evproc(S_exp *params, S_exp *body, S_exp *args,
              S_exp *lenv, S_exp *cenv);

S_exp *eval_inner(S_exp *cont, S_exp *result) {
    /* for (S_exp *ce = cont; !IS_NIL(ce); ce = cdr(ce)) {
        fprintf(stderr, " ");
        write_S(stderr, car(car(ce)));
        fprintf(stderr, " .");
    }
    fprintf(stderr, "\n"); */

    S_exp *exp = car(car(cont));
    S_exp *env = cdr(car(cont));
    cont = cdr(cont);

    switch (exp->type) {
    case PRIM: case PROC:
        result = exp;
        break;
    case SYM:
        if (exp == a_result)
            /* result = result */
            break;
        result = cdr(assoc(exp, env));
        break;
    case CONS:
        switch (car(exp)->type) {
        case PRIM:
            result = (car(exp)->u.op)(cdr(exp), env, cont, result);
            cont   = cdr(result);
            result = car(result);
            break;
        case PROC:
            result = evproc(car(car(exp)), car(cdr(car(exp))),
                            cdr(exp), car(cdr(cdr(car(exp)))), env);
            cont = cons(result, cont);
            result = nil;
            break;
        case SYM:
            if (car(exp) == a_result) {
                cont = cons(cons(cons(result, cdr(exp)), env), cont);
                break;
            }
            /* fallthrough */
        case CONS:
            result = eval(car(exp), env);
            if (IS_NIL(result)) {
                write_S(stderr, car(exp));
                error(": unknown command\n");
            }
            cont = cons(cons(cons(result, cdr(exp)), env), cont);
            break;
        default:
            error("Unknown car(exp) type %d\n", exp->type);
        }
        break;
    default:
        error("Unknown exp type %d\n", exp->type);
    }

    return cons(result, cont);
}

S_exp *eval(S_exp *exp, S_exp *env) {
    S_exp *cont = cons(cons(exp, env), nil), *result = nil;
    while (!IS_NIL(cont)) {
        S_exp *ir = eval_inner(cont, result);
        result = car(ir);
        cont   = cdr(ir);
    }
    return result;
}

S_exp *evlis(S_exp *exp, S_exp *env) {
    return (IS_NIL(exp) ? nil
            : cons(eval(car(exp), env), evlis(cdr(exp), env)));
}

S_exp *evlet(S_exp *pairs, S_exp *body, S_exp *env) {
    S_exp *procs = nil, *nenv = nil;

    /* TODO: make this work better, via the algorithm
     * 1. extend env with vars set to current values
     * 2. eval each value expression in that environment
     * 3. set, in-place, vars to evaluated values */

    for (; !IS_NIL(pairs); pairs = cdr(pairs)) {
        S_exp *val = eval(cdr(car(pairs)), env);
        nenv = cons(cons(car(car(pairs)), val), nenv);
        if (val->type == PROC)
            procs = cons(val, procs);
    }

    /* The hacky twist: lambdas get the inner env as their env! */
    for (; !IS_NIL(procs); procs = cdr(procs))
        procs->u.pair[0]->u.pair[1]->u.pair[1]->u.pair[0]
            = append(nenv, car(cdr(cdr(car(procs)))));

    return cons(body, append(nenv, env));
}

S_exp *evproc(S_exp *params, S_exp *body, S_exp *args,
              S_exp *lenv, S_exp *cenv) {
    /* TODO: allow more than one exp in a proc? maybe via macro? */

    if (IS_NIL(params))
        return cons(body, lenv);
    if (IS_ATOM(params))
        return cons(body, cons(cons(params, evlis(args, cenv)), lenv));
    return evproc(cdr(params), body, cdr(args),
                  cons(cons(car(params), eval(car(args), cenv)), lenv),
                  cenv);
}


/* Primitives. */
#define PRIM(name) S_exp *(__prim_##name)(S_exp *exp, S_exp *env, S_exp *cont, S_exp *result)

#define DECL(name, prim, env) ( \
    env = cons(cons(atom(name), mkprim((__prim_##prim))), env))

#define PRIMRET(result) do { return (cons((result), cont)); } while (0)

#define TAILCALL(eec) do { \
    cont = cons((eec), cont); \
    PRIMRET(nil); \
} while (0)

#define EVLIS(final) do { \
    cont = cons(cons(cons((final), nil), env), cont); \
    return nevlis(exp, env, cont); \
} while (0)

S_exp *mkprim(S_exp *(*fn)(S_exp *, S_exp *, S_exp *, S_exp *)) {
    S_exp *exp = malloc(sizeof(fn));
    exp->type = PRIM;
    exp->u.op = fn;
    return exp;
}

PRIM(quote)  { PRIMRET(car(exp)); }
PRIM(env)    { PRIMRET(env); }
PRIM(lambda) {
    S_exp *nl = cons(car(exp), cons(car(cdr(exp)), cons(env, nil)));
    nl->type = PROC;
    PRIMRET(nl);
}

S_exp *prim_cons, *prim_lcons, *prim_lrcons;
/* (lrcons . exp) == (cons *result* exp) [does not eval exp] */
PRIM(lrcons) { PRIMRET(cons(result, exp)); }
/* (lcons . exp) == (cons exp *result*) */
PRIM(lcons)  {
    cont = cons(cons(cons(prim_lrcons, result), env), cont);
    TAILCALL(cons(exp, env));
}
PRIM(cons)   {
    cont = cons(cons(cons(prim_lcons, car(exp)), env), cont);
    TAILCALL(cons(car(cdr(exp)), env));
}

S_exp *nevlis(S_exp *exp, S_exp *env, S_exp *cont) {
    for (; !IS_NIL(exp); exp = cdr(exp))
        cont = cons(cons(cons(prim_lcons, car(exp)), env), cont);

    PRIMRET(nil);
}

/* TODO: fix this one */
PRIM(let)  { TAILCALL(evlet(car(exp), car(cdr(exp)), env)); }

S_exp *prim_rcar;
PRIM(rcar) { PRIMRET(car(car(result))); }
PRIM(car)  { EVLIS(prim_rcar); }

S_exp *prim_rcdr;
PRIM(rcdr) { PRIMRET(cdr(car(result))); }
PRIM(cdr)  { EVLIS(prim_rcdr); }

S_exp *prim_req_p;
PRIM(req_p) { PRIMRET((IS_EQ(car(result), car(cdr(result))) ? tee : nil)); }
PRIM(eq_p)  { EVLIS(prim_req_p); }

S_exp *prim_reval;
PRIM(reval) { TAILCALL(cons(car(result), car(cdr(result)))); }
PRIM(eval)  { EVLIS(prim_reval); }

S_exp *prim_rif;
PRIM(rif)   {
    if (IS_NIL(result)) TAILCALL(cons(car(cdr(exp)), env));
    else                TAILCALL(cons(car(exp), env));
}
/* (if test truthy? falsy?) */
PRIM(if)    {
    cont = cons(cons(cons(prim_rif, cdr(exp)), env), cont);
    TAILCALL(cons(car(exp), env));
}

/* Initialization/main. */
S_exp *init() {
    nil      = atom("nil");
    tee      = atom("t");
    quote    = atom("quote");
    noeval   = atom("noeval");
    rest     = atom("&rest");
    a_result = atom("*result*");

    prim_lrcons = mkprim(__prim_lrcons);
    prim_lcons  = mkprim(__prim_lcons);
    prim_cons   = mkprim(__prim_cons);
    prim_rcar   = mkprim(__prim_rcar);
    prim_rcdr   = mkprim(__prim_rcdr);
    prim_req_p  = mkprim(__prim_req_p);
    prim_reval  = mkprim(__prim_reval);
    prim_rif    = mkprim(__prim_rif);

    S_exp *root_env = nil;
    DECL("car",    car, root_env);
    DECL("cdr",    cdr, root_env);
    DECL("cons",   cons, root_env);
    DECL("eq?",    eq_p, root_env);
    DECL("eval",   eval, root_env);
    DECL("lambda", lambda, root_env);
    DECL("if",     if, root_env);
    DECL("*env*",  env, root_env);
    DECL("let",    let, root_env);
    DECL("quote",  quote, root_env);
    return root_env;
}

int main(int argc, char **argv) {
    S_exp *root_env = init();

    FILE *inp = stdin;
    if (argc >= 2)
        inp = fopen(argv[1], "r");

    S_exp *root = read_S(inp);
    S_exp *result = eval(root, root_env);
    write_S(stdout, result);
    printf("\n");
    return 0;
}
