/* Copyright 2017 Bradley Jensen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include <stdio.h>
#include <ecl/ecl.h>

extern void init_lib_SHCL(cl_object);

int main(int argc, char **argv) {
    ecl_set_option(ECL_OPT_TRAP_SIGCHLD, 0);
    cl_boot(argc, argv);
    cl_object lib = read_VV(OBJNULL, init_lib_SHCL);
    printf("Exiting");
}
