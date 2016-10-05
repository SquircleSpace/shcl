#include <stdio.h>
#include <ecl/ecl.h>

extern void init_lib_SHCL(cl_object);

int main(int argc, char **argv) {
    ecl_set_option(ECL_OPT_TRAP_SIGCHLD, 0);
    cl_boot(argc, argv);
    cl_object lib = read_VV(OBJNULL, init_lib_SHCL);
    printf("Exiting");
}
