FROM links-handlers-base

#
# v8
#
RUN git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git /depot_tools
ENV PATH=/depot_tools:$PATH
RUN fetch v8
WORKDIR /v8
RUN git fetch --all
RUN git checkout -b v6.2 branch-heads/6.2

RUN gclient config https://chromium.googlesource.com/v8/v8
RUN gclient runhooks
RUN gclient sync

RUN ./build/install-build-deps.sh   --no-arm   --no-chromeos-fonts   --no-nacl --no-syms --no-prompt
RUN tools/dev/v8gen.py -vv x64.release

RUN find . -type f -print0 | xargs -0 sed -i 's!-Werror!-Wall!g'

# RUN ninja -C out.gn/x64.release d8
# (This fails with segmentation faults.)
# RUN make_bin "$CWD/$v8dir/out.gn/x64.release/d8" "d8"

#
# JavaScriptCore / webkit
#
WORKDIR /
#RUN svn checkout -q https://svn.webkit.org/repository/webkit/releases/WebKitGTK/webkit-2.18.0/
#WORKDIR webkit-2.18.0
#RUN Tools/Scripts/build-webkit --jsc-only --release
ADD https://webkitgtk.org/releases/webkitgtk-2.19.91.tar.xz /webkitgtk-2.19.91.tar.xz 
RUN tar xfJ webkitgtk-2.19.91.tar.xz 
WORKDIR /webkitgtk-2.19.91
RUN mkdir PerformanceTests && touch PerformanceTests/CMakeLists.txt
RUN cmake  -DPORT=JSCOnly -GNinja
# RUN ninja
# This fails with:
#
#   collect2: error: ld returned 1 exit status

#
# node.js
#
RUN apt-get install nodejs

#
# chakracore
#
# (TODO)

#
# spidermonkey
#
# (TODO)
