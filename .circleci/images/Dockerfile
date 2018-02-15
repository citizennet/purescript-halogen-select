# Using the most recent version of node by default
# Could also use node:alpine if size is an issue.
# Uses `stretch` for latest Debian
FROM node:stretch

MAINTAINER Thomas Honeyman

# To enable global installs it is necessary to change the location
# of node modules globally and use the node user. For more, see:
# https://github.com/nodejs/docker-node/issues/479#issuecomment-319446283
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global

USER node

# Install necessary tooling
RUN mkdir /home/node/.npm-global \
    && npm install -g purescript pulp bower

ENV PATH="/home/node/.npm-global/bin:${PATH}"
