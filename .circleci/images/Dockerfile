# Using the most recent version of node by default
# Could also use node:alpine if size is an issue.
FROM node:latest

MAINTAINER Thomas Honeyman

# To enable global installs it is necessary to change the location
# of node modules globally and use the node user. For more, see:
# https://github.com/nodejs/docker-node/issues/479#issuecomment-319446283
ENV NPM_CONFIG_PREFIX=~/.npm-global

USER node

# Install necessary tooling
RUN mkdir ~/.npm-global \
    && mkdir ~/app \
    && npm install -g purescript pulp bower

ENV PATH="~/.npm-global/bin:${PATH}"
