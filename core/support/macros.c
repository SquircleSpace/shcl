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

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>

int wifexited(int status) {
    return WIFEXITED(status);
}

int wexitstatus(int status) {
    return WEXITSTATUS(status);
}

int wifsignaled(int status) {
    return WIFSIGNALED(status);
}

int wtermsig(int status) {
    return WTERMSIG(status);
}

int wifstopped(int status) {
    return WIFSTOPPED(status);
}

int wstopsig(int status) {
    return WSTOPSIG(status);
}

int s_isreg(mode_t mode) {
    return S_ISREG(mode);
}

int s_isdir(mode_t mode) {
    return S_ISDIR(mode);
}

int s_ischr(mode_t mode) {
    return S_ISCHR(mode);
}

int s_isblk(mode_t mode) {
    return S_ISBLK(mode);
}

int s_isfifo(mode_t mode) {
    return S_ISFIFO(mode);
}

int s_islnk(mode_t mode) {
    return S_ISLNK(mode);
}

int s_issock(mode_t mode) {
    return S_ISSOCK(mode);
}
