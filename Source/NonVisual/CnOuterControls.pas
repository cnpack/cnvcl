{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2006 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnOuterControls;

{* |<PRE>
================================================================================
* 软件名称：系统功能组件包
* 单元名称：实现对外部程序控制单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnOuterControls.pas,v 1.1 2008/08/15 11:16:00 liuxiao Exp $
* 修改记录：2008.08.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, TlHelp32;

type
  TProcessInfo = record
    pHandle: Cardinal;
    pClassName: string;
    pText: string;
  end;

type
  TOnSendMessage = procedure(Sender: TObject; SndMsgResult: Cardinal) of object;
  TOnWindowChange = procedure(Sender: TObject) of object;

type
  TCnOuterControls = class(TComponent)
  private
    fProcessHandle: THandle;
    fTextList: TStringList;
    fHandleList: TStringList;
    fClassList: TStringList;
    fWindowCaption: string;
    fSM: Cardinal;
    fSLP: Cardinal;
    fSWP: Cardinal;
    fSMH: THandle;
    fOnSendMessage: TOnSendMessage;
    fOnWindowChange: TOnWindowChange;
    fPossibleWindow: TStringList;
    procedure SetProcessHandle(const Value: THandle);
    procedure SetWindowCaption(const Value: string);

  public
    constructor Create(AOwner: TComponent); override;
    {* 获取可用的窗口 }
    function GetPossibleWindows: TStringList;
    {* 获取指定 index 的组件信息 }
    function GetProcessControlInfo(index: Integer): TProcessInfo;
    {* 向指定组件发送消息 }
    procedure SendMessageToControl; overload;
    procedure SendMessageToControl(hWnd: THandle; Msg: Cardinal; WParam: Cardinal; LParam: Cardinal); overload;
  published
    {* 可用的窗口列表 }
    property PossibleWindows: TStringList read fPossibleWindow write fPossibleWindow;
    {* 发送消息时触发事件 }
    property OnSendMessage: TOnSendMessage read fOnSendMessage write fOnSendMessage;
    {* 窗口有改动时触发事件 }
    property OnWindowChange: TOnWindowChange read fOnWindowChange write fOnWindowChange;
    property SndMsgHandle: THandle read fSMH write fSMH;
    property SndMessage: Cardinal read fSM write fSM;
    property SndLParam: Cardinal read fSLP write fSLP;
    property SndWParam: Cardinal read fSWP write fSWP;
    property ProcessHandle: THandle read fProcessHandle write SetProcessHandle;
    {* 可用的句柄列表 }
    property HandleList: TStringList read fHandleList;
    {* 可用的类列表 }
    property ClassList: TStringList read fClassList;
    {* 可用的界面文本列表 }
    property TextList: TStringList read fTextList;
    {* 窗口标题 }
    property WindowCaption: string read fWindowCaption write SetWindowCaption;
  end;

var
  IHandleList: TStringList;
  IClassList: TStringList;
  ITextList: TStringList;

{* 此处使用回调函数来完成窗口的遍历与取值 }
function EnumChildWndProc(AhWnd: LongInt; AlParam: LParam): boolean; stdcall;
function EnumWindowsFunc(Handle: THandle; List: TStringList): boolean; stdcall;

implementation

function EnumChildWndProc(AhWnd: LongInt; AlParam: LParam): boolean; stdcall;
var
  WndClassName: array[0..511] of Char;
  WndCaption: array[0..511] of Char;
begin
  GetClassName(AhWnd, WndClassName, 512);   //获取控件名称
  GetWindowText(AhWnd, WndCaption, 512);    //获取控件标题
  IHandleList.Add(IntToStr(AhWnd));
  IClassList.Add(string(WndClassName));
  ITextList.Add(string(WndCaption));
  result := true;
end;

function EnumWindowsFunc(Handle: THandle; List: TStringList): boolean; stdcall;
var
  Caption: array[0..511] of Char;
begin
  if GetWindowText(Handle, Caption, 512) <> 0 then
  begin
    if List.IndexOf(Caption) = -1 then
      List.Add(Caption);
  end;
  result := true;
end;

{ TCnOuterControls }

constructor TCnOuterControls.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fTextList := TStringList.Create;
  fTextList.Clear;
  fHandleList := TStringList.Create;
  fHandleList.Clear;
  fClassList := TStringList.Create;
  fClassList.Clear;
  IHandleList := TStringList.Create;
  IHandleList.Clear;
  IClassList := TStringList.Create;
  IClassList.Clear;
  ITextList := TStringList.Create;
  ITextList.Clear;
  fPossibleWindow := TStringList.Create;
  fPossibleWindow.Clear;
  GetPossibleWindows;
end;

function TCnOuterControls.GetPossibleWindows: TStringList;
begin
  fPossibleWindow.Clear;
  EnumWindows(@EnumWindowsFunc, LParam(fPossibleWindow));
  result := fPossibleWindow;
end;

function TCnOuterControls.GetProcessControlInfo(
  index: Integer): TProcessInfo;
var
  piInfo: TProcessInfo;
begin
  piInfo.pHandle := 0;
  piInfo.pClassName := '';
  piInfo.pText := '';
  if fHandleList.Count - 1 < index then
  begin
    result := piInfo;
    Exit;
  end;
  piInfo.pHandle := StrToInt(fHandleList.Strings[index]);
  piInfo.pClassName := fClassList.Strings[index];
  piInfo.pText := fTextList.Strings[index];
  result := piInfo;
end;

procedure TCnOuterControls.SendMessageToControl;
var
  SndResult: Cardinal;
begin
  SndResult := SendMessage(fSMH, fSM, fSWP, fSLP);
  if Assigned(OnSendMessage) then
    OnSendMessage(self, SndResult);
end;

procedure TCnOuterControls.SendMessageToControl(hWnd: THandle; Msg, WParam,
  LParam: Cardinal);
var
  SndResult: Cardinal;
begin
  SndResult := SendMessage(hWnd, Msg, WParam, LParam);
  if Assigned(OnSendMessage) then
    OnSendMessage(self, SndResult);
end;

procedure TCnOuterControls.SetProcessHandle(const Value: THandle);
begin
  fProcessHandle := Value;
  IHandleList.Clear;
  IClassList.Clear;
  ITextList.Clear;
  if fProcessHandle <> 0 then EnumChildWindows(fProcessHandle, @EnumChildWndProc, 0);
  fTextList := ITextList;
  fHandleList := IHandleList;
  fClassList := IClassList;
  if Assigned(OnWindowChange) then
    OnWindowChange(self);
end;

procedure TCnOuterControls.SetWindowCaption(const Value: string);
begin
  fWindowCaption := Value;
  ProcessHandle := FindWindow(nil, PChar(fWindowCaption));
end;

end.

 