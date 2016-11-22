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
