FROM node:lts AS node
FROM ocaml/opam:debian-10-ocaml-4.12 as opam
FROM registry.access.redhat.com/ubi8/ubi:8.1

USER root

# copy node from node container and link commands
COPY --from=node /usr/local/lib/node_modules /usr/local/lib/node_modules
COPY --from=node /usr/local/bin/node /usr/local/bin/node
COPY --from=node /opt /opt
RUN ln -s /usr/local/lib/node_modules/npm/bin/npm-cli.js /usr/local/bin/npm \
  && ln -s /usr/local/lib/node_modules/npm/bin/npx-cli.js /usr/local/bin/npx \
  && ln -s /usr/local/bin/node /usr/local/bin/nodejs \
  && ln -s /opt/yarn-v*/bin/yarn /usr/local/bin/yarn \
  && ln -s /opt/yarn-v*/bin/yarnpkg /usr/local/bin/yarnpkg

# Avoid warnings by switching to noninteractive
ENV SIHL_ENV development

RUN yum makecache

# for some reason this does not work:
# RUN yum groupinstall 'Development Tools'

RUN yum -y install m4 wget gcc gmp mariadb-connector-c-devel openssl make patch unzip gcc diffutils git rsync zip tar nano curl wget
 
# add timezone
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime

# WTF: https://github.com/mirage/ocaml-cohttp/issues/675
RUN bash -c 'echo "http		80/tcp	www		# WorldWideWeb HTTP" >> /etc/services' \
  && bash -c 'echo "https		443/tcp	www		# WorldWideWeb HTTPS" >> /etc/services'

# install opam from opam image
COPY --from=opam /usr/bin/opam-2.0 /usr/bin/opam 
RUN chmod +x /usr/bin/opam

# opam user
RUN mkdir -p "/etc/sudoers.d" && \
  echo 'opam ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/opam && \
  chmod 440 /etc/sudoers.d/opam && \
  chown root:root /etc/sudoers.d/opam && \
  adduser --password '' opam && \
  passwd -l opam && \
  chown -R opam:opam /home/opam

USER opam
ENV HOME /home/opam
WORKDIR /home/opam

RUN mkdir .ssh && \
  chmod 700 .ssh 

# sandboxing requires bubblewrap, but we are in a container at build time anyway
# I wasn't able to install or find bubblewrap on rhel 8
# if we decide to build on the actual rhel 8 server, we probably should not do this
# otherwise malicious packages could access any files
RUN opam init -k local -a --bare --disable-sandboxing --debug-level=3
ENV OPAMYES=1 OPAMCONFIRMLEVEL=unsafe-yes OPAMERRLOGLEN=0 OPAMPRECISETRACKING=1
RUN opam switch create 4.12 --packages=ocaml-base-compiler.4.12.1
RUN opam pin add -k version ocaml-base-compiler 4.12.1
RUN opam install -y opam-depext 

COPY . /home/opam/app

RUN cd app \
  && repo_oxi=https://github.com/oxidizing \
  && repo_uzh=https://github.com/uzh \
  && opam pin add -yn sihl $repo_oxi/sihl.git \
  && opam pin add -yn sihl-cache $repo_oxi/sihl.git \
  && opam pin add -yn sihl-email $repo_oxi/sihl.git \
  && opam pin add -yn sihl-queue $repo_oxi/sihl.git \
  && opam pin add -yn sihl-storage $repo_oxi/sihl.git \ 
  && opam pin add -yn sihl-token $repo_oxi/sihl.git \ 
  && opam pin add -yn sihl-user $repo_oxi/sihl.git \
  && opam pin add -yn conformist $repo_oxi/conformist.git \
  && opam pin add -yn letters $repo_oxi/letters.git \
  && opam pin add -yn canary $repo_uzh/canary.git \
  && opam pin add -ywn guardian $repo_uzh/guardian.git \
  && opam pin add -yn pool . \
  && OPAMSOLVERTIMEOUT=180 opam depext -y pool \
  && eval $(opam env)

RUN cd app && opam install --deps-only -y .

RUN cd app && opam exec -- dune build --root .

RUN ./app/_build/default/pool/run/run.exe

