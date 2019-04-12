FROM ubuntu:16.04
RUN apt-get -y update && apt-get install -y \
  libgmp3-dev
ADD .stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-12.0/8.4.3/bin/extravagance /bin/
CMD ["/bin/extravagance", "/input", "/input_json_patches", "/output"]
