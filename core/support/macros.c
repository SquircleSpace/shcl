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
