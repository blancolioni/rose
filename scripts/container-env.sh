# What make-build-container and container-build share.  Source it from the
# top of the tree.
#
# CONTAINER_ENGINE chooses podman or docker; by default it is podman if
# that is installed, and docker if not.  ROSE_CONTAINER names the container
# (rose-build by default); both scripts also take a name.

CONTAINER_NAME=${ROSE_CONTAINER:-rose-build}

# Where make-build-container puts Syn and idle in the container.
CONTAINER_TOOLS=/opt/rose
CONTAINER_IDLE=$CONTAINER_TOOLS/idle/bin/idle

# The label that records the directory the container mounts, so that
# container-build can check that a tree is inside it.
CONTAINER_MOUNT_LABEL=rose.mount

if [ -z "$CONTAINER_ENGINE" ]; then
    if command -v podman > /dev/null 2>&1; then
        CONTAINER_ENGINE=podman
    elif command -v docker > /dev/null 2>&1; then
        CONTAINER_ENGINE=docker
    else
        echo "$0: neither podman nor docker is installed" >&2
        exit 1
    fi
fi

case $CONTAINER_ENGINE in
    podman|docker) ;;
    *) echo "$0: CONTAINER_ENGINE is $CONTAINER_ENGINE, not podman or docker" >&2
       exit 1 ;;
esac

# Rootless podman maps root in the container to the user who runs it, so
# files that a build writes to the mounted tree belong to that user.
# Docker's root is the host's root, so under docker the build runs as the
# user instead, with a HOME it can write to.
container_user_options () {
    if [ "$CONTAINER_ENGINE" = docker ]; then
        echo "--user `id -u`:`id -g` -e HOME=/tmp"
    fi
}

container_exists () {
    "$CONTAINER_ENGINE" container inspect "$1" > /dev/null 2>&1
}
