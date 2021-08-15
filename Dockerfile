ARG RESOLVER=lts-17.15
FROM fpco/stack-build:${RESOLVER} as build

RUN mkdir /build
WORKDIR /build

COPY . /build
RUN cp stack.docker.yaml stack.yaml
RUN stack build --only-snapshot
RUN stack build --only-dependencies
RUN stack build
RUN stack install


FROM ubuntu:20.04 as prod

RUN mkdir -p /app/ && chmod 0755 /app
RUN adduser --shell /bin/bash --uid 1000 --disabled-password --gecos "" stack

COPY install-packages.sh /install-packages.sh
RUN PACKAGES="\
    ca-certificates \
    " bash /install-packages.sh \
    && rm /install-packages.sh

COPY --from=build /build/install/jira-issue-history /app
RUN chmod 0755 /app/jira-issue-history

USER stack
WORKDIR /home/stack

ENTRYPOINT ["/app/jira-issue-history"]