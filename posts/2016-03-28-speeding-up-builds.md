---
title: Speeding up the automated building and testing of our Haskell projects
---

Alternate title: Arrest me for crimes against GNU Make.

I work for a company that uses Haskell for its primary application. We're quite fond of having automatic builds fired off for each push to a branch and in our Github pull requests, so we were using [CircleCI](http://circleci.com) for our builds. Our circle.yml looked a bit like this initially:

```yaml
machine:
  ruby:
    version: 2.1.7
  services:
    - postgresql
  node:
    version: 5.1.0

dependencies:
  pre:
    - sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    - echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | sudo tee /etc/apt/sources.list.d/fpco.list
    - sudo apt-get update && sudo apt-get install stack -y
    - cd frontend && npm install
    - cd frontend && ./node_modules/.bin/bower install
    - npm install -g purescript
    - gem install bundler --pre
    - cd sql && bundle install

  override:
    - stack setup
    - cd app && make frontend:
        timeout: 3000
        environment:
          UV_THREADPOOL_SIZE: 2
    - make dirty:
        pwd: app

test:
  override:
    - make sql
    - make -C sql migrate
    - stack test --jobs=1
```

However, I'm also a bit of a speed demon and impatient, so I was getting tired of the slow builds we had with CircleCI. 25-30 minutes for each build is just way too long when you're waiting for the green checkmark to review code or merge it. My first pass was attempting to figure out what could be parallelized and what could be cached. Here's what we added under `dependencies` for caching:

```yaml
  cache_directories:
    - "~/.stack"
    - ".stack-work"
    - "frontend/node_modules"
    - "frontend/bower_components"
    - "frontend/output"
```

But it wasn't good enough.

![Even with caching, our CircleCI builds were taking about 20 minutes.](/images/slow-build.png)

We got a dedicated server and put [drone.io](http://drone.io) on it instead of using CircleCI. We couldn't have afforded the enterprise version of CircleCI and using something we can modify ourselves had a lot of appeal. Drone uses Docker to manage the build environment and after getting our build working and tests passing inside of the Docker containers, I was able to get Drone tracking our Github stuff pretty quickly. This got us down to about 6 minutes in order to do the following:

1. Build frontend assets
2. Build two different Haskell projects and run their respective tests

Here's approximately what the `.drone.yml` looked like:

```yaml
build:
  image: app
  environment:
    - POSTGRES_PORT_5432_TCP_ADDR=localhost
  commands:
    - make tests

notify:
  slack:
    webhook_url: OUR_WEBHOOK_URL
    channel: dev
    username: drone
    template: >
      build #{{ build.number }} finished with a {{ build.status }} status. Commit message: {{build.message}} - See more at {{system.link_url}}/{{repo.owner}}/{{repo.name}}/{{build.number}}

compose:
  db:
    image: postgres
    environment:
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=password
  app:
    image: app
    environment:
      - POSTGRES_PORT_5432_TCP_ADDR=localhost
    depends_on:
      - db
```

Note that we used Drone's baked in Docker Compose'ish functionality so that we didn't have to also configure a PostgreSQL server in the same container. `make tests` ends up running:

```
	make -C sql/ test
	make -C lib/ test
	make -C app/ test
```

These tasks stood up and migrated the database, built our business logic library and ran the tests, and build our web app and ran the tests.

I wasn't satisfied though, so I realized that since Drone uses Docker containers and our Haskell projects use [Stackage](http://stackage.org) LTS for our package versions, all we needed to do was specify our current LTS resolver and build a bunch of dependencies we knew we'd need. Here's what I added to the Docker build container's Dockerfile in order to pre-build the dependencies:

```
# Setup Stack
RUN stack setup --resolver lts-5.8
# ADD global-stack.yaml ~/.stack/global-project/stack.yaml
# RUN stack setup --resolver lts-5.8

# Cache some deps
RUN stack --resolver lts-5.8 build lens-aeson yesod yesod-test esqueleto http-client free classy-prelude-yesod classy-prelude-conduit case-insensitive gravatar wreq xml-conduit warp hspec QuickCheck wai-logger persistent-postgresql HUnit uuid-aeson monad-logger mandrill email-validate yesod-auth yesod-newsfeed yesod-form haskell-src-exts cpphs polyparse xml-hamlet th-orphans either base-compat th-expand-syns th-lift MonadRandom
```

The fruit of my effort was:

![Screenshot of a recent build & test run taking only 2 minutes or so. I elided the branch name.](/images/fast-build.png)

In so doing, our build times dropped from 20 or 25 minutes with CircleCI, down to 2-3 minutes on our private [Drone](http://drone.io) server.

Here's the steps we took:

1. Added caching to our CircleCI build. This got us: 25 minutes -> 20 minutes.

2. Switched from CircleCI on their shared servers to Drone on a $100/month dedicated server. This got us: 20 minutes -> 6 minutes.

3. Started pre-building dependencies. This got us: 6 minutes -> 2 or 3 minutes.

Note that these 2 or 3 minutes isn't just building a Haskell project, that takes a couple seconds. About half the time is spent building frontend assets! Our test suite is relatively fast because we cache things like the Yesod application initialization. I believe we could get this down to a minute or two, but I don't want to muck about with our PureScript build chain. Even with two Haskell developers, we think the $100/month and time to get it working paid off quickly.

I would estimate Drone took _slightly_ longer than CircleCI originally did to get working initially, but I think the small difference is worth it and that it would've been faster for someone that dislikes Docker less.
