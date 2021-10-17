#include "chinnacc.h"
#include <stdarg.h>
#include <stdio.h>

// Takes in a printf style fmt string and variable args
// and returns the formatted string. The memstream fn opens a
// stream for writing to the buffer, which is dunamically allocatted
// with malloc and automatically growing as required. After the stream is closed with fclose,
// the first arg is updated to point to this allocated buffer
// essentially a way to create formatted strings not meant for printing
char *format(char *fmt, ...) {
    char *buf;
    size_t buflen;
    FILE *out = open_memstream(&buf, &buflen);

    va_list ap;
    va_start(ap, fmt);
    vfprintf(out, fmt, ap);
    va_end(ap);
    fclose(out);
    return buf;
}