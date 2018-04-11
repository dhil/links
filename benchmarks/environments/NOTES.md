Plan: build everything in layers

base image: debian:sid
next layer: development tools (opam, make, etc.)
            (links-handlers-base Dockerfile.base)
next layer: js runtime(s)
            (links-handlers-js Dockerfile.js)
next layer: links build
            (links-handlers-links Dockerfile.links)
next layer: test cases
            (links-handlers-tests Dockerfile.tests)
