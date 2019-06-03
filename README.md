# extravagance

1. [extravagance](#extravagance)
   1. [Usage](#usage)
      1. [Docker](#docker)
      1. [Binary](#binary)

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
