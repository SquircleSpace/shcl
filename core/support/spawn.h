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

#include <unistd.h>

struct shcl_fd_actions;
typedef struct shcl_fd_actions shcl_fd_actions;

shcl_fd_actions *make_shcl_fd_actions();
void destroy_shcl_fd_actions(shcl_fd_actions *actions);
void shcl_fd_actions_add_close(shcl_fd_actions *actions, int fd);
void shcl_fd_actions_add_dup2(shcl_fd_actions *actions, int fd1, int fd2);

int shcl_spawn(
    pid_t *pid,
    const char *path,
    int search,
    int working_directory_fd,
    shcl_fd_actions *fd_actions,
    char * const argv[],
    char * const envp[]);
