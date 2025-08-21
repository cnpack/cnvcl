CnVCL 中的例子工程，分 VCL、FMX 和 FPC 三大类，另有一 Linux 目录仅供各平台下的命令行测试加解密功能用。

VCL 和 FMX 的目录下的每个工程，要求既能用 Delphi 打开工程直接编译，也能用该目录下的编译脚本 bat 文件通过命令行正确编译。VCL 目录下的编译脚本是 BuildVclExamples.bat，FMX 目录下的编译脚本是 BuildFmxExamples.bat。

因此，对于每个工程，会有以下要求：

Delphi：指定工程的正确的搜索目录；高版本中添加合适前缀如 Vcl 或 Fmx 或 WinApi 等，以让低版本的 CnVCL 源码能够引用到正确文件。

命令行：使用正确的 cfg 文件，其中指定正确的搜索目录，及单元别名。注意单元别名同样是用于兼容不同的高低版本 Delphi 中不同的库文件名所用。

VCL 目录下的工程以 Delphi 7 编译运行为主。FMX 目录下的工程以 Delphi 10.3 Rio 编译运行为主。bat 文件内是如此指定的，如需要更改成其他版本的 Delphi，或更改成非默认安装目录下的 Delphi，需要手工修改 bat 文件里头上部的编译器路径及文件名，如：

SET DCC32="C:\Program Files\Borland\Delphi7\Bin\dcc32.exe"
SET DCCR_32="C:\Program Files\Embarcadero\Studio\20.0\bin\dcc32.exe"

至于 FPC 目录下，目前只支持 FPC 3.2.2 的简单命令行编译，没有兼容旧版的后顾之忧，因而相对简单。如需要更改成其他版本的 FPC，或更改成非默认安装目录下的 FPC，只需手工修改这里即可：

SET DCC32="C:\lazarus\fpc\3.2.2\bin\i386-win32\fpc.exe" 