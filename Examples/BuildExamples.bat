@ECHO OFF
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