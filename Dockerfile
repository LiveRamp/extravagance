FROM ubuntu:16.04
RUN apt-get -y update && apt-get install -y \
  libgmp3-dev
COPY .stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-13.16/8.6.4/bin/extravagance /bin/
COPY resources /bin/resources
WORKDIR /bin
CMD ["/bin/extravagance", "/input", "/input_json_patches", "/output"]
