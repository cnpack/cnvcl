{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnOTAUtils;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：Delphi/Lazarus 设计期组件包工具单元，类似于 CnWizUtils
* 单元作者：CnPack 开发组 CnPack 开发组 (master@cnpack.org)
* 备    注：该单元实现了一些设计期的 Delphi OTA 及 Lazarus 相关函数
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.24 V1.1
*               实现一部分 Lazarus 里的设计期功能
*           2006.08.19 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, {$IFDEF FPC} LazIDEIntf, ProjectIntf,
  CompOptsIntf, SrcEditorIntf, {$ELSE}
  ToolsAPI, {$IFDEF COMPILER6_UP} Variants, {$ENDIF} {$ENDIF}
  CnCommon;

{$IFDEF FPC}

function CnOtaGetCurrentProject: TLazProject;
{* 取当前工程 }

{$ELSE}

function CnOtaGetProjectGroup: IOTAProjectGroup;
{* 取当前工程组 }

function CnOtaGetCurrentProject: IOTAProject;
{* 取当前工程 }

{$ENDIF}

function CnOtaGetCurrentProjectFileName: string;
{* 取当前工程文件名称 }

{$IFDEF FPC}

function CnOtaGetActiveProjectOptions: TLazCompilerOptions;
{* 取当前工程选项 }

{$ELSE}

function CnOtaGetActiveProjectOptions(Project: IOTAProject = nil): IOTAProjectOptions;
{* 取当前工程选项 }

function CnOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
{* 取当前工程指定选项 }

{$ENDIF}

function CnOtaGetOutputDir: string;
{* 取当前工程输出目录 }

{$IFNDEF FPC}

function CnOtaGetFileNameOfModule(Module: IOTAModule;
  GetSourceEditorFileName: Boolean = False): string;
{* 取指定模块文件名，GetSourceEditorFileName 表示是否返回在代码编辑器中打开的文件}

function CnOtaGetCurrentModule: IOTAModule;
{* 取当前模块}

{$ENDIF}

function CnOtaGetFileNameOfCurrentModule(GetSourceEditorFileName: Boolean = False): string;
{* 取当前模块文件名}

function GetIdeRootDirectory: string;
{* 取得 IDE 根目录}

function CnOtaIsFileOpen(const FileName: string): Boolean;
{* 判断文件是否打开 }

function IsCpp(const FileName: string): Boolean;
{* 判断是否.Cpp文件}

function CnOtaReplaceToActualPath(const Path: string): string;
{* 将 $(DELPHI) 这样的符号替换为 Delphi 所在路径}

{$IFNDEF FPC}
{$IFDEF SUPPORT_OTA_PROJECT_CONFIGURATION}
function CnOtaGetActiveProjectOptionsConfigurations(Project: IOTAProject = nil): IOTAProjectOptionsConfigurations;
{* 取当前工程配置选项，2009 后才有效}
{$ENDIF}
{$ENDIF}

implementation

{ Other DesignTime Utils Routines }

const
  SCnIDEPathMacro = '{$DELPHI}';

{$IFDEF FPC}

function CnOtaGetCurrentProject: TLazProject;
begin
  if Assigned(LazarusIDE) then
    Result := LazarusIDE.ActiveProject
  else
    Result := nil;
end;

{$ELSE}

// 取当前工程组
function CnOtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  I: Integer;
begin
  Result := nil;
  Supports(BorlandIDEServices, IOTAModuleServices, IModuleServices);
  if IModuleServices <> nil then
  begin
    for I := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[I];
      if Supports(IModule, IOTAProjectGroup, Result) then
        Break;
    end;
  end;
end;

// 取当前工程
function CnOtaGetCurrentProject: IOTAProject;
var
  IProjectGroup: IOTAProjectGroup;
begin
  Result := nil;

  IProjectGroup := CnOtaGetProjectGroup;
  if not Assigned(IProjectGroup) then
    Exit;

  try
    Result := IProjectGroup.ActiveProject;
  except
    Result := nil;
  end;
end;

{$ENDIF}

// 取当前工程文件名称
function CnOtaGetCurrentProjectFileName: string;
{$IFNDEF FPC}
var
  CurrentProject: IOTAProject;
{$ENDIF}
begin
{$IFDEF FPC}
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
    Result := LazarusIDE.ActiveProject.ProjectInfoFile
  else
    Result := '';
{$ELSE}
  CurrentProject := CnOtaGetCurrentProject;
  if Assigned(CurrentProject) then
    Result := CurrentProject.FileName
  else
    Result := '';
{$ENDIF}
end;

{$IFDEF FPC}

function CnOtaGetActiveProjectOptions: TLazCompilerOptions;
var
  BMs: TLazProjectBuildModes;
  BM: TLazProjectBuildMode;
  ID: string;
  Idx: Integer;
begin
  Result := nil;
  if Assigned(LazarusIDE) and Assigned(LazarusIDE.ActiveProject) then
  begin
    BMs := LazarusIDE.ActiveProject.LazBuildModes;
    ID := LazarusIDE.ActiveProject.ActiveBuildModeID;
    Idx := BMs.IndexOf(ID);
    if Idx >= 0 then
    begin
      BM := BMs.BuildModes[Idx];
      if BM <> nil then
        Result := BM.LazCompilerOptions;
    end;
  end;
end;

{$ELSE}

// 取当前工程选项
function CnOtaGetActiveProjectOptions(Project: IOTAProject = nil): IOTAProjectOptions;
begin
  Result := nil;
  if Assigned(Project) then
  begin
    Result:=Project.ProjectOptions;
    Exit;
  end;

  Project := CnOtaGetCurrentProject;
  if Assigned(Project) then
    Result := Project.ProjectOptions;
end;

// 取当前工程指定选项
function CnOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
var
  ProjectOptions: IOTAProjectOptions;
begin
  Result := False;
  Value := '';

  ProjectOptions := CnOtaGetActiveProjectOptions;
  if Assigned(ProjectOptions) then
  begin
    Value := ProjectOptions.Values[Option];
    Result := True;
  end;
end;

{$ENDIF}

// 取当前工程输出目录
function CnOtaGetOutputDir: string;
var
  ProjectDir: string;
{$IFDEF FPC}
  Options: TLazCompilerOptions;
{$ELSE}
  OutputDir: Variant;
{$ENDIF}
begin
  ProjectDir := _CnExtractFileDir(CnOtaGetCurrentProjectFileName);

{$IFDEF FPC}
  Options := CnOtaGetActiveProjectOptions;
  if Assigned(Options) then
    Result := LinkPath(ProjectDir, Options.UnitOutputDirectory) 
  else
    Result := ProjectDir;
{$ELSE}
  if CnOtaGetActiveProjectOption('OutputDir', OutputDir) then
    Result := LinkPath(ProjectDir, OutputDir)
  else
    Result := ProjectDir;
{$ENDIF}
end;

{$IFNDEF FPC}

// 取指定模块文件名，GetSourceEditorFileName 表示是否返回在代码编辑器中打开的文件
function CnOtaGetFileNameOfModule(Module: IOTAModule;
  GetSourceEditorFileName: Boolean): string;
var
  I: Integer;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
begin
  Result := '';
  if Assigned(Module) then
  begin
    if not GetSourceEditorFileName then
      Result := Module.FileName
    else
    begin
      for I := 0 to Module.GetModuleFileCount - 1 do
      begin
        Editor := Module.GetModuleFileEditor(I);
        if Supports(Editor, IOTASourceEditor, SourceEditor) then
        begin
          Result := Editor.FileName;
          Break;
        end;
      end;
    end;
  end;
end;

// 取当前模块
function CnOtaGetCurrentModule: IOTAModule;
var
  iModuleServices: IOTAModuleServices;
begin
  Result := nil;
  Supports(BorlandIDEServices, IOTAModuleServices, iModuleServices);
  if iModuleServices <> nil then
    Result := iModuleServices.CurrentModule;
end;

{$ENDIF}

// 取当前模块文件名，注意 Lazarus 下只支持源文件名
function CnOtaGetFileNameOfCurrentModule(GetSourceEditorFileName: Boolean): string;
{$IFDEF FPC}
var
  Editor: TSourceEditorInterface;
{$ENDIF}
begin
{$IFDEF FPC}
  Result := '';
  if SourceEditorManagerIntf = nil then Exit;

  Editor := SourceEditorManagerIntf.ActiveEditor;
  
  if Assigned(Editor) then
    Result := Editor.FileName;
{$ELSE}
  Result := CnOtaGetFileNameOfModule(CnOtaGetCurrentModule, GetSourceEditorFileName);
{$ENDIF}
end;

// 取得 IDE 根目录
function GetIdeRootDirectory: string;
begin
  Result := _CnExtractFilePath(_CnExtractFileDir(Application.ExeName));
end;

{$IFNDEF FPC}

// 取模块编辑器
function CnOtaGetFileEditorForModule(Module: IOTAModule; Index: Integer): IOTAEditor;
begin
  Result := nil;
  if not Assigned(Module) then Exit;
  try
    // BCB 5 下为一个简单的单元调用 GetModuleFileEditor(1) 会出错
    {$IFDEF BCB5}
    if IsCpp(Module.FileName) and (Module.GetModuleFileCount = 2) and (Index = 1) then
      Index := 2;
    {$ENDIF}
    Result := Module.GetModuleFileEditor(Index);
  except
    Result := nil; // 在 IDE 释放时，可能会有异常发生
  end;
end;

{$ENDIF}

// 判断文件是否打开
function CnOtaIsFileOpen(const FileName: string): Boolean;
var
{$IFDEF FPC}
  Editor: TSourceEditorInterface;
{$ELSE}
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
{$ENDIF}
  I: Integer;
begin
  Result := False;

{$IFDEF FPC}
  if (LazarusIDE = nil) or (SourceEditorManagerIntf = nil) then Exit;

  // 遍历所有打开的编辑器
  for I := 0 to SourceEditorManagerIntf.SourceEditorCount - 1 do
  begin
    Editor := SourceEditorManagerIntf.SourceEditors[I];
    
    // 比较文件路径（跨平台安全）
    if CompareText(FileName, Editor.FileName) = 0 then
    begin
      Result := True;
      Exit;
    end;
  end;
{$ELSE}
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ModuleServices = nil then Exit;

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
  begin
    for I := 0 to Module.GetModuleFileCount-1 do
    begin
      FileEditor := CnOtaGetFileEditorForModule(Module, I);
      Assert(Assigned(FileEditor));

      Result := CompareText(FileName, FileEditor.FileName) = 0;
      if Result then
        Exit;
    end;
  end;
{$ENDIF}
end;

// 判断是否.Cpp文件
function IsCpp(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := UpperCase(_CnExtractFileExt(FileName));
  Result := (FileExt = '.CPP');
end;

{$IFNDEF FPC}
{$IFDEF SUPPORT_OTA_PROJECT_CONFIGURATION}
// * 取当前工程配置选项，2009 后才有效
function CnOtaGetActiveProjectOptionsConfigurations
  (Project: IOTAProject): IOTAProjectOptionsConfigurations;
var
  ProjectOptions: IOTAProjectOptions;
begin
  ProjectOptions := CnOtaGetActiveProjectOptions(Project);
  if ProjectOptions <> nil then
    if Supports(ProjectOptions, IOTAProjectOptionsConfigurations, Result) then
      Exit;

  Result := nil;
end;
{$ENDIF}
{$ENDIF}

// 将 $(DELPHI) 这样的符号替换为 Delphi 所在路径
function CnOtaReplaceToActualPath(const Path: string): string;
var
{$IFDEF FPC}
  Options: TLazCompilerOptions;
{$ELSE}
{$IFDEF COMPILER6_UP}
  Vars: TStringList;
  I: Integer;
{$IFDEF DELPHI2011_UP}
  BC: IOTAProjectOptionsConfigurations;
{$ENDIF}
{$ENDIF}
{$ENDIF}
begin
{$IFDEF COMPILER6_UP}
  Result := Path;
  Vars := TStringList.Create;
  try
    GetEnvironmentVars(Vars, True);
    for I := 0 to Vars.Count - 1 do
      Result := StringReplace(Result, '$(' + Vars.Names[I] + ')',
        Vars.Values[Vars.Names[I]], [rfReplaceAll, rfIgnoreCase]);
    {$IFDEF DELPHI2011_UP}
      BC := CnOtaGetActiveProjectOptionsConfigurations(nil);
      if BC <> nil then
      begin
        if BC.GetActiveConfiguration <> nil then
        begin
          Result := StringReplace(Result, '$(Config)',
            BC.GetActiveConfiguration.GetName, [rfReplaceAll, rfIgnoreCase]);
    {$IFDEF DELPHI2012_UP}
          Result := StringReplace(Result, '$(Platform)',
            BC.GetActiveConfiguration.GetPlatform, [rfReplaceAll, rfIgnoreCase]);
    {$ENDIF}
        end;
      end;
    {$ENDIF}
  finally
    Vars.Free;
  end;   
{$ELSE}
  {$IFDEF FPC}
  Result := Path;
  Options := CnOtaGetActiveProjectOptions;
  if Options <> nil then
  begin
    Result := StringReplace(Result, '$(TargetOS)',
      Options.TargetOS, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '$(TargetCPU)',
      Options.TargetCPU, [rfReplaceAll, rfIgnoreCase]);
  end;
  {$ELSE}
  // Delphi5 下不支持环境变量
  Result := StringReplace(Path, SCnIDEPathMacro, MakeDir(GetIdeRootDirectory),
    [rfReplaceAll, rfIgnoreCase]);
  {$ENDIF}
{$ENDIF}
end;

end.
