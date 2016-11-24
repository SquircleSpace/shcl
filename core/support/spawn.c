#include "spawn.h"

#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

enum shcl_fd_action {
    shcl_fd_action_close,
    shcl_fd_action_dup2,
};

typedef struct shcl_fd_action_node {
    struct shcl_fd_action_node *next;
    enum shcl_fd_action action;
    int fd_arg_1;
    int fd_arg_2;
} shcl_fd_action_node;

struct shcl_fd_actions {
    shcl_fd_action_node *head;
    shcl_fd_action_node *tail;
};

shcl_fd_actions *make_shcl_fd_actions()
{
    shcl_fd_actions *result = malloc(sizeof(shcl_fd_actions));
    result->head = NULL;
    result->tail = NULL;
    return result;
}

void destroy_shcl_fd_actions(shcl_fd_actions *actions)
{
    shcl_fd_action_node *node = actions->head;
    while (node) {
        shcl_fd_action_node *next = node->next;
        free(node);
        node = next;
    }
    free(actions);
}

void shcl_fd_actions_add_node(shcl_fd_actions *actions, shcl_fd_action_node *node)
{
    if (actions->head) {
        assert(actions->tail);
        actions->tail->next = node;
        actions->tail = node;
    } else {
        assert(!actions->tail);
        actions->head = node;
        actions->tail = node;
    }
}

void shcl_fd_actions_add_close(shcl_fd_actions *actions, int fd)
{
    shcl_fd_action_node *node = malloc(sizeof(shcl_fd_action_node));
    node->next = NULL;
    node->action = shcl_fd_action_close;
    node->fd_arg_1 = fd;
    node->fd_arg_2 = -1;
    shcl_fd_actions_add_node(actions, node);
}

void shcl_fd_actions_add_dup2(shcl_fd_actions *actions, int fd1, int fd2)
{
    shcl_fd_action_node *node = malloc(sizeof(shcl_fd_action_node));
    node->next = NULL;
    node->action = shcl_fd_action_dup2;
    node->fd_arg_1 = fd1;
    node->fd_arg_2 = fd2;
    shcl_fd_actions_add_node(actions, node);
}

void shcl_fd_actions_take(shcl_fd_actions *actions)
{
    for (shcl_fd_action_node *node = actions->head; NULL != node; node = node->next) {
        switch (node->action) {
        case shcl_fd_action_close:
            if (0 != close(node->fd_arg_1)) {
                _exit(127);
            }
            break;

        case shcl_fd_action_dup2:
            if (0 > dup2(node->fd_arg_1, node->fd_arg_2)) {
                _exit(127);
            }
            break;
        }
    }
}

int shcl_spawn(
    pid_t *pid,
    const char *path,
    int search,
    int working_directory_fd,
    shcl_fd_actions *fd_actions,
    char * const argv[],
    char * const envp[])
{
    pid_t forked_pid = fork();
    if (forked_pid < 0) {
        return 0;
    } else if (forked_pid > 0) {
        *pid = forked_pid;
        return 1;
    }

    if (0 > fchdir(working_directory_fd)) {
        _exit(127);
    }

    shcl_fd_actions_take(fd_actions);

    if (0 != clearenv()) {
        _exit(127);
    }

    for (int i = 0; NULL != envp[i]; ++i) {
        char *env_str = envp[i];
        if (0 != putenv(env_str)) {
            _exit(127);
        }
    }

    if (search) {
        execvp(path, argv);
    } else {
        execv(path, argv);
    }
    _exit(127);
}