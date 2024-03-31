# Based on https://github.com/containers/buildah/blob/main/tests/bud/multi-stage-builds/Dockerfile.extended

#define X 1

FROM busybox:latest AS builder
ENV 'BUILD'"_LOGLEVEL"="5" A=1
RUN touch /tmp/preCommit
ENTRYPOINT /bin/sleep 600
ENV "OPENSHIFT_BUILD_NAME"="mydockertest-1" "OPENSHIFT_BUILD_NAMESPACE"="default"
LABEL "io.openshift.build.name"="mydockertest-1" "io.openshift.build.namespace"="default"

#define Y 1

FROM builder
ENV "BUILD_LOGLEVEL"="5"
RUN touch /tmp/postCommit

#define Z 1

FROM builder
ENV "BUILD_LOGLEVEL"="5"
RUN echo "$BUILD_LOGLEVEL"
