/*
 * An interactive interpreter for Datalog programs.
 *
 * John D. Ramsdell
 * Copyright (C) 2004 The MITRE Corporation
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <lua.h>
#include "datalog.h"
#include <time.h>

#define STDIN_NAME "-"

#ifdef PACKAGE_NAME
const char package[] = PACKAGE_NAME;
#else
const char *package = NULL;
#endif

#ifdef VERSION
const char version[] = VERSION;
#else
const char version[] = "version unknown";
#endif

static void
print_version(const char *program)
{
  if (package)
    program = package;
  fprintf(stderr, "%s %s\n", program, version);
}

static void
usage(const char *prog)
{
  fprintf(stderr,
	  "Usage: %s [options] [file]\n"
	  "Options:\n"
	  "  -o file -- output to file (default is standard output)\n"
	  "  -i      -- enter interactive mode after loading file\n"
	  "  -v      -- print version information\n"
	  "  -h      -- print this message\n"
	  "Use - as a file name to specify standard input\n",
	  prog);
}

/* A datalog answer printer. */

static int
print_answers(dl_answers_t a)
{
  int i, j, n;
  const char *p;
  size_t s;
  if (!a)
    return 0;
  n = dl_getpredarity(a);
  p = dl_getpred(a);
  s = dl_getpredlen(a);
  if (n == 0) {
    dl_putlconst(stdout, p, s);	/* Print zero arity predicate. */
    printf(".\n");
  }
  else if (n == 2 && !strcmp(p, "=") && s == 1) {
    for (i = 0; dl_getconst(a, i, 0); i++) {
      dl_putlconst(stdout, dl_getconst(a, i, 0),
		   dl_getconstlen(a, i, 0));
      printf(" = ");   /* Print equality answers in infix notation. */
      dl_putlconst(stdout, dl_getconst(a, i, 1),
		   dl_getconstlen(a, i, 1));
      printf(".\n");
    }
  }
  else {		 /* Print all other non-zero arity answers. */
    for (i = 0; dl_getconst(a, i, 0); i++) {
      dl_putlconst(stdout, p, s);
      printf("(");
      dl_putlconst(stdout, dl_getconst(a, i, 0),
		   dl_getconstlen(a, i, 0));
      for (j = 1; j < n; j++) {
	printf(", ");
	dl_putlconst(stdout, dl_getconst(a, i, j),
		     dl_getconstlen(a, i, j));
      }
      printf(").\n");
    }
  }
  dl_free(a);
  return 0;
}

/* Load a Datalog file. */

#define BUFFER_SIZE (1 << 10)

typedef struct {
  FILE *in;
  const char *filename;
  char buffer[BUFFER_SIZE];
} loadfile_t;

static const char *
getbuf(void *data, size_t *size)
{
  loadfile_t *lf = (loadfile_t *)data;
  FILE *in = lf->in;
  size_t n = fread(lf->buffer, 1, BUFFER_SIZE, in);
  if (n < BUFFER_SIZE && ferror(in)) {
    perror(lf->filename);
    exit(1);
  }
  *size = n;
  return n > 0 ? lf->buffer : NULL;
}

static void
loaderror(void *data, int lineno, int colno, const char *msg)
{
  loadfile_t *lf = (loadfile_t *)data;
  const char *filename = lf->filename;
  fprintf(stderr, "%s:%d:%d: %s\n", filename, lineno, colno, msg);
}

static int			/* Close all but stdin. */
leave(int rc, FILE *in)
{
  if (in != stdin)
    fclose(in);
  return rc;
}

static int
loadfile(dl_db_t db, FILE *in, const char *filename)
{				/* Read, evaluate, and print */
  int rc;			/* for a file. */
  double timefunc1, timefunc2, timeload1, timeload2, timeask1, timeask2;
  timefunc1 = clock();
  dl_answers_t a;
  loadfile_t lf;
  lf.filename = filename;
  lf.in = in;
  timeload1 = clock();
  rc = dl_load(db, getbuf, loaderror, &lf); /* Read. */
  timeload2 = clock();
  if (rc)
    return leave(rc, in);
  timeask1 = clock();
  rc = dl_ask(db, &a);		/* Eval. */
  timeask2 = clock();
  if (rc)
    return leave(rc, in);
  timefunc2 = clock();
  printf("loadfile: %f\n", (timefunc2-timefunc1)/CLOCKS_PER_SEC);
  printf("load: %f\n", (timeload2-timeload1)/CLOCKS_PER_SEC);
  printf("dl_ask: %f\n", (timeask2-timeask1)/CLOCKS_PER_SEC);
  return leave(print_answers(a), in); /* Print. */
}

/* Read a line of text for the interaction loop. */

#define LINE_SIZE (1 << 7)

typedef struct {
  dl_db_t db;
  int done;			/* Is current input complete? */
  int again;	/* Has some of current input already been returned? */
  int eof;			/* Was EOF found during reading? */
  char buffer[LINE_SIZE];
} linebuffer_t;

static const char *
getline(void *data, size_t *size)
{
  linebuffer_t *lb = (linebuffer_t *)data;
  char *buf = lb->buffer;
  int nofiles = lb->again;
  FILE *in;
  size_t n;
  if (lb->done)		     /* We're done, so return end of input. */
    return NULL;

  if (lb->again)		/* Show prompt. */
    printf(">");
  lb->again = 1;
  printf("> ");
  fflush(stdout);
  lb->eof = 0;

  if (!fgets(buf, LINE_SIZE, stdin)) {
    lb->done = 1;
    lb->eof = 1;		/* EOF or error found. */
    return NULL;
  }
  n = strlen(buf);
  if (n == 0) {			/* Cannot happen, right? */
    lb->done = 1;
    lb->eof = 1;
    return NULL;
  }
  else if (!nofiles && buf[0] == '=') {	/* File loading requested. */
    if (buf[n - 1] != '\n') {	/* Line not finished. */
      fprintf(stderr, "file name too long\n");
      return NULL;		/* Give up. */
    }
    buf[n - 1] = 0;
    in = fopen(buf + 1, "r");	/* Only active at the beginning */
    if (!in)			/* of input--!nofile check.*/
      perror(buf + 1);
    else
      loadfile(lb->db, in, buf + 1);
    lb->done = 1;		/* Consider the current input */
    return NULL;		/* as containing no data. */
  }
  else if (buf[n - 1] != '\n') { /* Line not finished. */
    lb->done = 0;		/* Continuation needed. */
    *size = n;
  }
  else if (n > 1 && buf[n - 2] == '\\') { /* Line continued. */
    lb->done = 0;		/* Lines that end in back slash */
   *size = n - 2;		/* are continued. */
  }
  else {			/* Complete input found, in other */
    lb->done = 1;		/* words, an unescapped newline */
    *size = n;			/* terminated input was found. */
  }
  return buf;
}

static void
lineerror(void *data, int lineno, int colno, const char *msg)
{
  (void)data; (void)lineno; (void)colno;
  fprintf(stderr, "%s\n", msg);
}

static int
interp(dl_db_t db)
{				/* Read, evaluate, and print */
  dl_answers_t a;		/* interaction loop. */
  int rc;
  linebuffer_t lb;
  lb.db = db;
  if (package)			/* Show welcome message. */
    printf("%s %s\n\n", package, version);
  do {
    lb.again = lb.done = 0;
    rc = dl_load(db, getline, lineerror, &lb); /* Read. */
    if (!rc) {
      rc = dl_ask(db, &a);	/* Eval. */
      if (!rc) {
	if (print_answers(a))	/* Print. */
	  fprintf(stderr, "Internal error in print_answers");
      }
      else {
	fprintf(stderr, "Internal error in dl_ask\n");
	return 1;
      }
    }
  }
  while (!lb.eof);		/* Until EOF on input. */
  printf("\n");
  if (ferror(stdin)) {
    perror("interp");
    return 1;
  }
  else
    return 0;
}

int
main(int argc, char **argv)
{
  extern char *optarg;
  extern int optind;

  int interact = 0;
  char *input = NULL;
  char *output = NULL;

  FILE *in = NULL;

  dl_db_t db;
  int rc;

  for (;;) {
    int c = getopt(argc, argv, "o:ivh");
    if (c == -1)
      break;
    switch (c) {
    case 'o':
      output = optarg;
      break;
    case 'i':
      interact = 1;
      break;
    case 'v':
      print_version(argv[0]);
      return 0;
    case 'h':
      usage(argv[0]);
      return 0;
    default:
      usage(argv[0]);
      return 1;
    }
  }

  switch (argc - optind) {
  case 0:			/* Use stdin */
    interact = 1;
    break;
  case 1:
    input = argv[optind];
    if (strcmp(STDIN_NAME, input)) {
      in = fopen(input, "r");
      if (!in) {
	perror(input);
	return 1;
      }
    }
    else
      in = stdin;
    break;
  default:
    fprintf(stderr, "Bad arg count\n");
    usage(argv[0]);
    return 1;
  }

  if (output && !freopen(output, "w", stdout)) {
    perror(output);
    return 1;
  }

  db = dl_open();
  if (!db) {
    fprintf(stderr, "Internal error\n");
    return 1;
  }

  rc = 0;
  if (in)
    rc = loadfile(db, in, input);
  if (interact && !rc)
    rc = interp(db);
  dl_close(db);			/* Allow valgrind leak check. */
  return rc;
}
