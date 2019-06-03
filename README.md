# extravagance

1. [extravagance](#extravagance)
   1. [Usage](#usage)
      1. [Docker](#docker)
      1. [Binary](#binary)
   1. [Building](#building)

## Usage

### Docker

The docker image expects that you mount the input Java patches into `/input`, JSON patches into `/input_json`, and the Java files to be patched into `/output`. For example:

```bash
docker run \
    -v/users/svc-jenkins/code/dist_types/src/main/java/:/input \
    -v/users/svc-jenkins/code/dist_types/src/main/json/:/input_json_patches \
    -v/users/svc-jenkins/code/dist_types/src/main/generated:/output \
    liveramp/extravagance:latest
```

The Java files will be modified **in-place**.

### Binary

The `extravagance` binary itself accepts three arguments, which are, respectively, the path of the Java patches, the path of the JSON patches, and the path to the Java files which are to be patched. For example:

```bash
extravagance \
    /users/svc-jenkins/code/dist_types/src/main/java/ \
    /users/svc-jenkins/code/dist_types/src/main/json/ \
    /users/svc-jenkins/code/dist_types/src/main/generated
```

## Building

First, run `stack docker pull` to download the docker image used to run the compilation process. In theory, you should now be able to just run `bin/build-docker` to produce a Docker image locally. The process may randomly crash, possibly due to running out of memory. You should be able to restart it and it will continue where it left off.

To build a binary for your development system, simply run `stack build .`.
