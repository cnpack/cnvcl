{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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
* 单元名称：OTA 设计期工具单元，类似于 CnWizUtils
* 单元作者：CnPack 开发组 CnPack 开发组 (master@cnpack.org)
* 备    注：该单元实现了一些设计期的 OTA 相关函数
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2006.08.19 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, ToolsAPI,
  {$IFDEF COMPILER6_UP}Variants, {$ENDIF}
  CnCommon;
  
function CnOtaGetProjectGroup: IOTAProjectGroup;
{* 取当前工程组 }

function CnOtaGetCurrentProject: IOTAProject;
{* 取当前工程 }

function CnOtaGetCurrentProjectFileName: string;
{* 取当前工程文件名称 }

function CnOtaGetActiveProjectOptions(Project: IOTAProject = nil): IOTAProjectOptions;
{* 取当前工程选项 }

function CnOtaGetActiveProjectOption(const Option: string; var Value: Variant): Boolean;
{* 取当前工程指定选项 }

function CnOtaGetOutputDir: string;
{* 取当前工程输出目录 }

function CnOtaGetFileNameOfModule(Module: IOTAModule;
  GetSourceEditorFileName: Boolean = False): string;
{* 取指定模块文件名，GetSourceEditorFileName 表示是否返回在代码编辑器中打开的文件}

function CnOtaGetFileNameOfCurrentModule(GetSourceEditorFileName: Boolean = False): string;
{* 取当前模块文件名}

function CnOtaGetCurrentModule: IOTAModule;
{* 取当前模块}

function GetIdeRootDirectory: string;
{* 取得 IDE 根目录}

function CnOtaIsFileOpen(const FileName: string): Boolean;
{* 判断文件是否打开 }

function IsCpp(const FileName: string): Boolean;
{* 判断是否.Cpp文件}

function CnOtaReplaceToActualPath(const Path: string): string;
{* 将 $(DELPHI) 这样的符号替换为 Delphi 所在路径}

{$IFDEF SUPPORT_OTA_PROJECT_CONFIGURATION}
function CnOtaGetActiveProjectOptionsConfigurations(Project: IOTAProject = nil): IOTAProjectOptionsConfigurations;
{* 取当前工程配置选项，2009 后才有效}
{$ENDIF}

implementation

{ Other DesignTime Utils Routines }

const
  SCnIDEPathMacro = '{$DELPHI}';

// 取当前工程组
function CnOtaGetProjectGroup: IOTAProjectGroup;
var
  IModuleServices: IOTAModuleServices;
  IModule: IOTAModule;
  i: Integer;
begin
  Result := nil;
  Supports(BorlandIDEServices, IOTAModuleServices, IModuleServices);
  if IModuleServices <> nil then
    for i := 0 to IModuleServices.ModuleCount - 1 do
    begin
      IModule := IModuleServices.Modules[i];
      if Supports(IModule, IOTAProjectGroup, Result) then
        Break;
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

// 取当前工程文件名称
function CnOtaGetCurrentProjectFileName: string;
var
  CurrentProject: IOTAProject;
begin
  CurrentProject := CnOtaGetCurrentProject;
  if Assigned(CurrentProject) then
    Result := CurrentProject.FileName
  else
    Result := '';
end;

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

// 取当前工程输出目录
function CnOtaGetOutputDir: string;
var
  ProjectDir: string;
  OutputDir: Variant;
begin
  ProjectDir := _CnExtractFileDir(CnOtaGetCurrentProjectFileName);
  if CnOtaGetActiveProjectOption('OutputDir', OutputDir) then
    Result := LinkPath(ProjectDir, OutputDir)
  else
    Result := ProjectDir;
end;

// 取指定模块文件名，GetSourceEditorFileName 表示是否返回在代码编辑器中打开的文件
function CnOtaGetFileNameOfModule(Module: IOTAModule;
  GetSourceEditorFileName: Boolean): string;
var
  i: Integer;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
begin
  Result := '';
  if Assigned(Module) then
    if not GetSourceEditorFileName then
      Result := Module.FileName
    else
      for i := 0 to Module.GetModuleFileCount - 1 do
      begin
        Editor := Module.GetModuleFileEditor(i);
        if Supports(Editor, IOTASourceEditor, SourceEditor) then
        begin
          Result := Editor.FileName;
          Break;
        end;
      end;
end;

// 取当前模块文件名
function CnOtaGetFileNameOfCurrentModule(GetSourceEditorFileName: Boolean): string;
begin
  Result := CnOtaGetFileNameOfModule(CnOtaGetCurrentModule, GetSourceEditorFileName);
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

// 取得 IDE 根目录
function GetIdeRootDirectory: string;
begin
  Result := _CnExtractFilePath(_CnExtractFileDir(Application.ExeName));
end;

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

// 判断文件是否打开
function CnOtaIsFileOpen(const FileName: string): Boolean;
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i: Integer;
begin
  Result := False;

  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if ModuleServices = nil then Exit;

  Module := ModuleServices.FindModule(FileName);
  if Assigned(Module) then
  begin
    for i := 0 to Module.GetModuleFileCount-1 do
    begin
      FileEditor := CnOtaGetFileEditorForModule(Module, i);
      Assert(Assigned(FileEditor));

      Result := CompareText(FileName, FileEditor.FileName) = 0;
      if Result then
        Exit;
    end;
  end;
end;

// 判断是否.Cpp文件
function IsCpp(const FileName: string): Boolean;
var
  FileExt: string;
begin
  FileExt := UpperCase(_CnExtractFileExt(FileName));
  Result := (FileExt = '.CPP');
end;

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

// 将 $(DELPHI) 这样的符号替换为 Delphi 所在路径
function CnOtaReplaceToActualPath(const Path: string): string;
{$IFDEF COMPILER6_UP}
var
  Vars: TStringList;
  i: Integer;
{$IFDEF DELPHI2011_UP}
  BC: IOTAProjectOptionsConfigurations;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF COMPILER6_UP}
  Result := Path;
  Vars := TStringList.Create;
  try
    GetEnvironmentVars(Vars, True);
    for i := 0 to Vars.Count - 1 do
      Result := StringReplace(Result, '$(' + Vars.Names[i] + ')',
        Vars.Values[Vars.Names[i]], [rfReplaceAll, rfIgnoreCase]);
    {$IFDEF DELPHI2011_UP}
      BC := CnOtaGetActiveProjectOptionsConfigurations(nil);
      if BC <> nil then
        if BC.GetActiveConfiguration <> nil then
        begin
          Result := StringReplace(Result, '$(Config)',
            BC.GetActiveConfiguration.GetName, [rfReplaceAll, rfIgnoreCase]);
    {$IFDEF DELPHI2012_UP}
          Result := StringReplace(Result, '$(Platform)',
            BC.GetActiveConfiguration.GetPlatform, [rfReplaceAll, rfIgnoreCase]);
    {$ENDIF}
        end;
    {$ENDIF}
  finally
    Vars.Free;
  end;   
{$ELSE}
  // Delphi5 下不支持环境变量
  Result := StringReplace(Path, SCnIDEPathMacro, MakeDir(GetIdeRootDirectory),
    [rfReplaceAll, rfIgnoreCase]);
{$ENDIF}
end;

end.
