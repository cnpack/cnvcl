program RtlUtils;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  UnitRtlUtils in 'UnitRtlUtils.pas' {FormRtlUtils},
  CnRTL in '..\..\..\Source\Common\CnRTL.pas'
{$IFDEF USE_JCL},
  Jcl8087 in '..\..\..\cnwizards\Source\ThirdParty\JCL\Jcl8087.pas',
  JclAnsiStrings in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclAnsiStrings.pas',
  JclBase in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclBase.pas',
  JclConsole in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclConsole.pas',
  JclDateTime in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclDateTime.pas',
  JclDebug in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclDebug.pas',
  JclFileUtils in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclFileUtils.pas',
  JclHookExcept in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclHookExcept.pas',
  JclIniFiles in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclIniFiles.pas',
  JclLogic in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclLogic.pas',
  JclMath in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclMath.pas',
  JclPeImage in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclPeImage.pas',
  JclRegistry in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclRegistry.pas',
  JclResources in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclResources.pas',
  JclSecurity in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclSecurity.pas',
  JclShell in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclShell.pas',
  JclStreams in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclStreams.pas',
  JclStringConversions in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclStringConversions.pas',
  JclStrings in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclStrings.pas',
  JclSynch in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclSynch.pas',
  JclSysInfo in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclSysInfo.pas',
  JclSysUtils in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclSysUtils.pas',
  JclTD32 in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclTD32.pas',
  JclWideStrings in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclWideStrings.pas',
  JclWin32 in '..\..\..\cnwizards\Source\ThirdParty\JCL\JclWin32.pas',
  Snmp in '..\..\..\cnwizards\Source\ThirdParty\JCL\Snmp.pas'
{$ENDIF};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormRtlUtils, FormRtlUtils);
  Application.Run;
end.
