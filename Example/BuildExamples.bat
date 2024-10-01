@ECHO OFF
REM 例子一般在 cfg/dof/dproj 的配置信息中将所需单元的相对路径加入搜索路径，而尽量不直接加入工程文件中，避免维护麻烦。
CD VCL
CALL BuildVclExamples.bat
CD ..
CD FMX
CALL BuildFmxExamples.bat
CD ..
CD FPC
CALL BuildFpcExamples.bat
CD ..
ECHO Examples Build Complete.