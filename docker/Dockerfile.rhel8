FROM node:lts AS node
FROM ocaml/opam:debian-10-ocaml-4.12 as opam
FROM redhat/ubi8:8.7


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

# this is a port from the debian based ocaml 4.12 docker image
RUN yum clean all
# gmp-devel and libev-devel should be installed as well, but they don't seem to exist
# maybe they can be installed by log in in?
RUN yum -y install m4 wget gmp gcc mariadb-connector-c-devel openssl make patch unzip gcc diffutils git rsync zip tar nano curl wget sudo bzip2 pkgconf-pkg-config sqlite-devel
RUN ln -fs /usr/share/zoneinfo/Europe/Zurich /etc/localtime 
COPY --from=opam /usr/bin/opam-2.0 /usr/bin/opam-2.0 
RUN ln /usr/bin/opam-2.0 /usr/bin/opam 
RUN mkdir -p /etc/sudoers.d
RUN touch /etc/sudoers.d/opam 
RUN chmod 440 /etc/sudoers.d/opam 
RUN chown root:root /etc/sudoers.d/opam 
RUN adduser --uid 1000 --password '' opam 
RUN usermod -aG wheel opam
RUN passwd -d opam 
RUN chown -R opam:opam /home/opam 
USER opam
ENV HOME=/home/opam
WORKDIR /home/opam
RUN mkdir .ssh 
RUN chmod 700 .ssh 
RUN touch /home/opam/.opamrc-nosandbox 
RUN touch /home/opam/opam-sandbox-disable 
RUN chmod a+x /home/opam/opam-sandbox-disable 
RUN sudo mv /home/opam/opam-sandbox-disable /usr/bin/opam-sandbox-disable 
RUN touch /home/opam/.opamrc-sandbox 
RUN touch /home/opam/opam-sandbox-enable 
RUN chmod a+x /home/opam/opam-sandbox-enable 
RUN sudo mv /home/opam/opam-sandbox-enable /usr/bin/opam-sandbox-enable 
RUN git config --global user.email "docker@example.com" 
RUN git config --global user.name "Docker" 
RUN opam-sandbox-disable 
COPY --from=opam /home/opam/opam-repository /home/opam/opam-repository
RUN opam init -k local -a /home/opam/opam-repository --bare --disable-sandboxing
RUN rm -rf .opam/repo/default/.git 
ENV OPAMYES=1 OPAMCONFIRMLEVEL=unsafe-yes OPAMERRLOGLEN=0 OPAMPRECISETRACKING=1
RUN opam switch create 4.12 --packages=ocaml-base-compiler.4.12.1 
RUN opam update 
RUN opam pin add -k version ocaml-base-compiler 4.12.1 
RUN opam install -y opam-depext 

# we really don't want to build from source, why does 'yum install libev-devel' not work?
# these dependencies are needed to build libev from source
RUN sudo yum -y install autoconf automake libtool 
RUN wget http://dist.schmorp.de/libev/libev-4.33.tar.gz -O libev.tar.gz \
  && tar -xvzf libev.tar.gz \
  && cd libev-4.33 \
  && chmod +x autogen.sh \
  && chmod +x configure \
  && ./autogen.sh \
  && ./configure \
  && make \
  && make check \
  && sudo make install

# we really don't want to build from source, why does 'yum install gmp-devel' not work?
# these dependencies are needed to build gmp from source
RUN sudo yum -y install xz
RUN wget https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz -O gmp-6.2.1.tar.xz \
  && tar -xvf gmp-6.2.1.tar.xz \
  && cd gmp-6.2.1 \
  && chmod +x configure \
  && ./configure \
  && make \
  && make check \
  && sudo make install 


WORKDIR /home/opam/app
COPY . /home/opam/app

RUN repo_oxi=https://github.com/oxidizing \
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
  && opam pin add -yn pool . 

RUN opam install --deps-only --with-test -y .

RUN opam exec -- dune build --root .

RUN ldd _build/default/pool/run/run.exe

# it finds everything except for libev, should work at runtime
RUN opam config exec -- dune exec --root . pool/run/run.exe 