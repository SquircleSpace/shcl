# This file is machine generated.  Do not edit it!
{ fetchurl }:
let
  qlReleases =
    {
      "alexandria" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/alexandria/2018-12-10/alexandria-20181210-git.tgz";
          sha256 = "0dg0gr7cgrrl70sq0sbz8i1zcli54bqg4x532wscz3156xrl2588";
        };
        name = "alexandria";
        archiveName = "alexandria-20181210-git.tgz";
        archiveSize = 51197;
        archiveMD5 = "2a7530a412cd94a56b6d4e5864fb8819";
        archiveContentSHA1 = "ec06ed88438934caf0be71870d349894b3ac5778";
        prefix = "alexandria-20181210-git";
        systemFiles = [
          "alexandria.asd"
        ];
      };
      "anaphora" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/anaphora/2018-02-28/anaphora-20180228-git.tgz";
          sha256 = "1bd2mvrxdf460wqrmg93lrvrjzvhbxjq8fcpvh24afx6573g2d41";
        };
        name = "anaphora";
        archiveName = "anaphora-20180228-git.tgz";
        archiveSize = 6151;
        archiveMD5 = "a884be2d820c0bc7dc59dea7ffd72731";
        archiveContentSHA1 = "0b3438fc4eb8e96a60092f0357fc15a2d0b19f7a";
        prefix = "anaphora-20180228-git";
        systemFiles = [
          "anaphora.asd"
        ];
      };
      "babel" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/babel/2017-12-27/babel-20171227-git.tgz";
          sha256 = "166y6j9ma1vxzy5bcwnbi37zwgn2zssx5x1q7zr63kyj2caiw2rf";
        };
        name = "babel";
        archiveName = "babel-20171227-git.tgz";
        archiveSize = 253935;
        archiveMD5 = "8ea39f73873847907a8bb67f99f16ecd";
        archiveContentSHA1 = "221c2b5e9378355934005cb441ea1cb3b2965590";
        prefix = "babel-20171227-git";
        systemFiles = [
          "babel.asd"
        ];
      };
      "bordeaux-threads" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/bordeaux-threads/2018-07-11/bordeaux-threads-v0.8.6.tgz";
          sha256 = "1q3b9dbyz02g6iav5rvzml7c8r0iad9j5kipgwkxj0b8qijjzr1y";
        };
        name = "bordeaux-threads";
        archiveName = "bordeaux-threads-v0.8.6.tgz";
        archiveSize = 21941;
        archiveMD5 = "f959d3902694b1fe6de450a854040f86";
        archiveContentSHA1 = "5c918c98a216edadefe6fbb2f1c8b33d43c77547";
        prefix = "bordeaux-threads-v0.8.6";
        systemFiles = [
          "bordeaux-threads.asd"
        ];
      };
      "cffi" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/cffi/2018-12-10/cffi_0.20.0.tgz";
          sha256 = "1jal7r0dqp0sc5wj8a97xjlvfvayymdp1w3172hdxfppddnhhm8z";
        };
        name = "cffi";
        archiveName = "cffi_0.20.0.tgz";
        archiveSize = 260085;
        archiveMD5 = "94a8b377cf1ac7d8fc73dcc98f3420c6";
        archiveContentSHA1 = "cb1c4b36403f38290c37812b3cb62fc3f1f6510e";
        prefix = "cffi_0.20.0";
        systemFiles = [
          "cffi-grovel.asd"
          "cffi-toolchain.asd"
          "cffi.asd"
        ];
      };
      "cl-ansi-text" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/cl-ansi-text/2015-08-04/cl-ansi-text-20150804-git.tgz";
          sha256 = "112w7qg8yp28qyc2b5c7km457krr3xksxyps1icmgdpqf9ccpn2i";
        };
        name = "cl-ansi-text";
        archiveName = "cl-ansi-text-20150804-git.tgz";
        archiveSize = 5876;
        archiveMD5 = "70aa38b40377a5e89a7f22bb68b3f796";
        archiveContentSHA1 = "2de105750d54136d99a566bbf98d57c85088fb90";
        prefix = "cl-ansi-text-20150804-git";
        systemFiles = [
          "cl-ansi-text.asd"
        ];
      };
      "cl-colors" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/cl-colors/2018-03-28/cl-colors-20180328-git.tgz";
          sha256 = "0anrb3zsi03dixfsjz92y06w93kpn0d0c5142fhx72f5kafwvc4a";
        };
        name = "cl-colors";
        archiveName = "cl-colors-20180328-git.tgz";
        archiveSize = 14566;
        archiveMD5 = "5e59ea59b32a0254df9610a5662ae2ec";
        archiveContentSHA1 = "08d7a2af682802fce159e47ff3511112f24e892a";
        prefix = "cl-colors-20180328-git";
        systemFiles = [
          "cl-colors.asd"
        ];
      };
      "cl-fad" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/cl-fad/2018-04-30/cl-fad-20180430-git.tgz";
          sha256 = "175v6y32q6qpc8axacf8nw44pmsw7a6r476w0f01cp1gwvpis1cs";
        };
        name = "cl-fad";
        archiveName = "cl-fad-20180430-git.tgz";
        archiveSize = 24609;
        archiveMD5 = "005c1b7b376fc60dea72574d2acdc704";
        archiveContentSHA1 = "e389592fd6c0af849f4f420657cd784036c44bc5";
        prefix = "cl-fad-20180430-git";
        systemFiles = [
          "cl-fad.asd"
        ];
      };
      "cl-ppcre" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/cl-ppcre/2019-05-21/cl-ppcre-20190521-git.tgz";
          sha256 = "0p6jcvf9afnsg80a1zqsp7fyz0lf1fxzbin7rs9bl4i6jvm0hjqx";
        };
        name = "cl-ppcre";
        archiveName = "cl-ppcre-20190521-git.tgz";
        archiveSize = 155009;
        archiveMD5 = "a980b75c1b386b49bcb28107991eb4ec";
        archiveContentSHA1 = "d6593d8f842bcf6af810ff93c6c02b757bd49ecf";
        prefix = "cl-ppcre-20190521-git";
        systemFiles = [
          "cl-ppcre.asd"
        ];
      };
      "closer-mop" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/closer-mop/2019-05-21/closer-mop-20190521-git.tgz";
          sha256 = "1j7654srnlihndvqg934ibwxmky1s6mnza3pz8d5ky3vb63ymwmv";
        };
        name = "closer-mop";
        archiveName = "closer-mop-20190521-git.tgz";
        archiveSize = 22696;
        archiveMD5 = "fb909e32a9eb5c897a8f3db64414b81d";
        archiveContentSHA1 = "d00e211fb81a65dd5d9dea5a5ced76317d82c3cc";
        prefix = "closer-mop-20190521-git";
        systemFiles = [
          "closer-mop.asd"
        ];
      };
      "fset" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/fset/2017-10-19/fset-20171019-git.tgz";
          sha256 = "07qxbj40kmjknmvvb47prj81mpi6j39150iw57hlrzdhlndvilwg";
        };
        name = "fset";
        archiveName = "fset-20171019-git.tgz";
        archiveSize = 107932;
        archiveMD5 = "dc8de5917c513302dd0e135e6c133978";
        archiveContentSHA1 = "d53c56499c817bce4d486b1fc9611fe32b2c2eb1";
        prefix = "fset-20171019-git";
        systemFiles = [
          "fset.asd"
        ];
      };
      "let-plus" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/let-plus/2017-11-30/let-plus-20171130-git.tgz";
          sha256 = "1v8rp3ab6kp6v5kl58gi700wjs4qgmkxxkmhx2a1i6b2z934xkx7";
        };
        name = "let-plus";
        archiveName = "let-plus-20171130-git.tgz";
        archiveSize = 11126;
        archiveMD5 = "cd92097d436a513e7d0eac535617ca08";
        archiveContentSHA1 = "d3cbcee04b712d12d182f8308760da0cfb5d654c";
        prefix = "let-plus-20171130-git";
        systemFiles = [
          "let-plus.asd"
        ];
      };
      "lisp-namespace" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/lisp-namespace/2017-11-30/lisp-namespace-20171130-git.tgz";
          sha256 = "0vxk06c5434kcjv9p414yk23gs4rkibfq695is9y7wglck31fz2j";
        };
        name = "lisp-namespace";
        archiveName = "lisp-namespace-20171130-git.tgz";
        archiveSize = 9762;
        archiveMD5 = "d3052a13db167c6a53487f31753b7467";
        archiveContentSHA1 = "e84b47c0b146849eeb5f624cc2c491e36f62be8a";
        prefix = "lisp-namespace-20171130-git";
        systemFiles = [
          "lisp-namespace.asd"
        ];
      };
      "misc-extensions" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/misc-extensions/2015-06-08/misc-extensions-20150608-git.tgz";
          sha256 = "0pkvi1l5djwpvm0p8m0bcdjm61gxvzy0vgn415gngdixvbbchdqj";
        };
        name = "misc-extensions";
        archiveName = "misc-extensions-20150608-git.tgz";
        archiveSize = 25456;
        archiveMD5 = "ef8a05dd4382bb9d1e3960aeb77e332e";
        archiveContentSHA1 = "3f22977672a040f72b46d49fa680757deeff6a12";
        prefix = "misc-extensions-20150608-git";
        systemFiles = [
          "misc-extensions.asd"
        ];
      };
      "mt19937" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/mt19937/2011-02-19/mt19937-1.1.1.tgz";
          sha256 = "1iw636b0iw5ygkv02y8i41lh7xj0acglv0hg5agryn0zzi2nf1xv";
        };
        name = "mt19937";
        archiveName = "mt19937-1.1.1.tgz";
        archiveSize = 5551;
        archiveMD5 = "54c63977b6d77abd66ebe0227b77c143";
        archiveContentSHA1 = "268d9a0d1dc870bd409bf9a85f80ff39840efd49";
        prefix = "mt19937-1.1.1";
        systemFiles = [
          "mt19937.asd"
        ];
      };
      "prove" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/prove/2017-11-30/prove-20171130-git.tgz";
          sha256 = "13dmnnlk3r9fxxcvk6sqq8m0ifv9y80zgp1wg63nv1ykwdi7kyar";
        };
        name = "prove";
        archiveName = "prove-20171130-git.tgz";
        archiveSize = 878875;
        archiveMD5 = "630df4367537f799570be40242f8ed52";
        archiveContentSHA1 = "67eba1cb93bce0eb45d6ff02396eec04a3f439a6";
        prefix = "prove-20171130-git";
        systemFiles = [
          "prove.asd"
        ];
      };
      "slime" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/slime/2019-01-07/slime-v2.23.tgz";
          sha256 = "1ml602yq5s38x0syg0grik8i4h01jw06yja87vpkjl3mkxqvxvky";
        };
        name = "slime";
        archiveName = "slime-v2.23.tgz";
        archiveSize = 809590;
        archiveMD5 = "726724480d861d97e8b58bc8f9f27697";
        archiveContentSHA1 = "26ab236fbf4d52de1a5107c346cb687143bff73d";
        prefix = "slime-v2.23";
        systemFiles = [
          "swank.asd"
        ];
      };
      "trivial-features" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/trivial-features/2016-12-04/trivial-features-20161204-git.tgz";
          sha256 = "0i2zyc9c7jigljxll29sh9gv1fawdsf0kq7s86pwba5zi99q2ij2";
        };
        name = "trivial-features";
        archiveName = "trivial-features-20161204-git.tgz";
        archiveSize = 10654;
        archiveMD5 = "07497e3fd92e68027a96f877cfe62bd4";
        archiveContentSHA1 = "380404e85a690e8a6595b3e25d501caac88b4026";
        prefix = "trivial-features-20161204-git";
        systemFiles = [
          "trivial-features.asd"
        ];
      };
      "trivial-garbage" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/trivial-garbage/2019-05-21/trivial-garbage-20190521-git.tgz";
          sha256 = "0yhb7rkrbcfgghwvbw13nvmr86v19ka6qb53j8n89c7r270d8fdl";
        };
        name = "trivial-garbage";
        archiveName = "trivial-garbage-20190521-git.tgz";
        archiveSize = 10438;
        archiveMD5 = "38fb70797069d4402c6b0fe91f4ca5a8";
        archiveContentSHA1 = "d9013ebd6a0ea3bd5a23dc9368fb0e318d651fcd";
        prefix = "trivial-garbage-20190521-git";
        systemFiles = [
          "trivial-garbage.asd"
        ];
      };
      "trivial-gray-streams" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/trivial-gray-streams/2018-10-18/trivial-gray-streams-20181018-git.tgz";
          sha256 = "0a1dmf7m9zbv3p6f5mzb413cy4fz9ahaykqp3ik1a98ivy0i74iv";
        };
        name = "trivial-gray-streams";
        archiveName = "trivial-gray-streams-20181018-git.tgz";
        archiveSize = 8024;
        archiveMD5 = "0a9f564079dc41ce10d7869d82cc0952";
        archiveContentSHA1 = "27c3dc766e7c8fed3051403e0c38468a235f6acf";
        prefix = "trivial-gray-streams-20181018-git";
        systemFiles = [
          "trivial-gray-streams.asd"
        ];
      };
      "uiop" = {
        archive = fetchurl {
          url = "http://beta.quicklisp.org/archive/uiop/2019-05-21/uiop-3.3.3.tgz";
          sha256 = "1r89bqjmz1919l3wlmd32p416jzpacy3glkhiwnf1crkja27iagm";
        };
        name = "uiop";
        archiveName = "uiop-3.3.3.tgz";
        archiveSize = 99847;
        archiveMD5 = "64d561117f048ad8621eff7a6173d65e";
        archiveContentSHA1 = "c044515fda0bcb9d12c1b53ba47f1aee03031f49";
        prefix = "uiop-3.3.3";
        systemFiles = [
          "uiop.asd"
        ];
      };
    };
  qlSystems =
    {
      "alexandria" = {
        release = qlReleases."alexandria";
        name = "alexandria";
        systemFileName = "alexandria";
        requiredSystems = [
        ];
      };
      "anaphora" = {
        release = qlReleases."anaphora";
        name = "anaphora";
        systemFileName = "anaphora";
        requiredSystems = [
        ];
      };
      "babel" = {
        release = qlReleases."babel";
        name = "babel";
        systemFileName = "babel";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."trivial-features"
        ];
      };
      "bordeaux-threads" = {
        release = qlReleases."bordeaux-threads";
        name = "bordeaux-threads";
        systemFileName = "bordeaux-threads";
        requiredSystems = [
          qlSystems."alexandria"
        ];
      };
      "cffi" = {
        release = qlReleases."cffi";
        name = "cffi";
        systemFileName = "cffi";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."babel"
          qlSystems."trivial-features"
        ];
      };
      "cffi-grovel" = {
        release = qlReleases."cffi";
        name = "cffi-grovel";
        systemFileName = "cffi-grovel";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."cffi"
          qlSystems."cffi-toolchain"
        ];
      };
      "cffi-toolchain" = {
        release = qlReleases."cffi";
        name = "cffi-toolchain";
        systemFileName = "cffi-toolchain";
        requiredSystems = [
          qlSystems."cffi"
        ];
      };
      "cl-ansi-text" = {
        release = qlReleases."cl-ansi-text";
        name = "cl-ansi-text";
        systemFileName = "cl-ansi-text";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."cl-colors"
        ];
      };
      "cl-colors" = {
        release = qlReleases."cl-colors";
        name = "cl-colors";
        systemFileName = "cl-colors";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."let-plus"
        ];
      };
      "cl-fad" = {
        release = qlReleases."cl-fad";
        name = "cl-fad";
        systemFileName = "cl-fad";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."bordeaux-threads"
        ];
      };
      "cl-ppcre" = {
        release = qlReleases."cl-ppcre";
        name = "cl-ppcre";
        systemFileName = "cl-ppcre";
        requiredSystems = [
        ];
      };
      "closer-mop" = {
        release = qlReleases."closer-mop";
        name = "closer-mop";
        systemFileName = "closer-mop";
        requiredSystems = [
        ];
      };
      "fset" = {
        release = qlReleases."fset";
        name = "fset";
        systemFileName = "fset";
        requiredSystems = [
          qlSystems."misc-extensions"
          qlSystems."mt19937"
        ];
      };
      "let-plus" = {
        release = qlReleases."let-plus";
        name = "let-plus";
        systemFileName = "let-plus";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."anaphora"
        ];
      };
      "lisp-namespace" = {
        release = qlReleases."lisp-namespace";
        name = "lisp-namespace";
        systemFileName = "lisp-namespace";
        requiredSystems = [
          qlSystems."alexandria"
        ];
      };
      "misc-extensions" = {
        release = qlReleases."misc-extensions";
        name = "misc-extensions";
        systemFileName = "misc-extensions";
        requiredSystems = [
        ];
      };
      "mt19937" = {
        release = qlReleases."mt19937";
        name = "mt19937";
        systemFileName = "mt19937";
        requiredSystems = [
        ];
      };
      "prove" = {
        release = qlReleases."prove";
        name = "prove";
        systemFileName = "prove";
        requiredSystems = [
          qlSystems."alexandria"
          qlSystems."cl-ansi-text"
          qlSystems."cl-colors"
          qlSystems."cl-ppcre"
        ];
      };
      "swank" = {
        release = qlReleases."slime";
        name = "swank";
        systemFileName = "swank";
        requiredSystems = [
        ];
      };
      "trivial-features" = {
        release = qlReleases."trivial-features";
        name = "trivial-features";
        systemFileName = "trivial-features";
        requiredSystems = [
        ];
      };
      "trivial-garbage" = {
        release = qlReleases."trivial-garbage";
        name = "trivial-garbage";
        systemFileName = "trivial-garbage";
        requiredSystems = [
        ];
      };
      "trivial-gray-streams" = {
        release = qlReleases."trivial-gray-streams";
        name = "trivial-gray-streams";
        systemFileName = "trivial-gray-streams";
        requiredSystems = [
        ];
      };
      "uiop" = {
        release = qlReleases."uiop";
        name = "uiop";
        systemFileName = "uiop";
        requiredSystems = [
        ];
      };
    };
in { inherit qlSystems qlReleases; }
