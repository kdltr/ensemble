CHICKEN_REPOSITORY_PATH="`pwd`:`pwd`/repo:`csi -R chicken.platform -p '(car (repository-path))'`"
export CHICKEN_REPOSITORY_PATH

export CHICKEN_INSTALL_REPOSITORY=`pwd`/repo
mkdir -p $CHICKEN_INSTALL_REPOSITORY
