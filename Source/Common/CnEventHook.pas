{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2015 CnPack 开发组                       }
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

unit CnEventHook;
{ |<PRE>
================================================================================
* 软件名称：CnPack IDE 专家包
* 单元名称：对象事件挂接单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元用来挂接对象的事件
* 开发平台：PWin7 + Delphi 7
* 兼容测试：
* 本 地 化：该单元中的字符串支持本地化处理方式
* 单元标识：$Id$
* 修改记录：2015.07.10
*               实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Controls, TypInfo;

type
  TCnEventHook = class
  {* 挂接对象事件处理的实现类}
  private
    FObject: TObject;
    FEventName: string;
    FOldData: Pointer;
    FOldCode: Pointer;
    FNewData: Pointer;
    FNewCode: Pointer;
    FHooked: Boolean;
    FTrampolineData: TObject;
    FTrampoline: Pointer;
  public
    constructor Create(AObject: TObject; const AEventName: string;
      NewData: Pointer; NewCode: Pointer);
    {* 构造函数，传入待挂接的对象，待挂接的事件名，新事件处理程序的函数地址，
      新事件处理程序的对象。构造后自动挂接}
    destructor Destroy; override;
    {* 析构函数，自动取消挂接}

    procedure HookEvent;
    {* 挂接事件处理程序}
    procedure UnhookEvent;
    {* 取消挂接事件处理程序}

    property Hooked: Boolean read FHooked;
    {* 当前是否已挂接}
    property EventName: string read FEventName;
    {* 待挂接的事件名}

    property TrampolineData: TObject read FTrampolineData;
    {* 旧有事件处理程序的对象}
    property Trampoline: Pointer read FTrampoline;
    {* 旧有事件处理程序的入口地址}
  end;

implementation

{ TCnEventHook }

constructor TCnEventHook.Create(AObject: TObject;
  const AEventName: string; NewData, NewCode: Pointer);
begin
  FObject := AObject;
  FEventName := AEventName;
  FNewData := NewData;
  FNewCode := NewCode;

  HookEvent;
end;

destructor TCnEventHook.Destroy;
begin
  UnhookEvent;
  inherited;
end;

procedure TCnEventHook.HookEvent;
var
  Method: TMethod;
begin
  if Hooked then
    Exit;

  try
    Method := GetMethodProp(FObject, FEventName);
  except
    Exit;  // No EventName
  end;

  FOldCode := Method.Code;
  FOldData := Method.Data;

  FTrampolineData := TObject(FOldData);
  FTrampoline := FOldCode;

  Method.Code := FNewCode;
  Method.Data := FNewData;
  SetMethodProp(FObject, FEventName, Method);

  FHooked := True;
end;

procedure TCnEventHook.UnhookEvent;
var
  Method: TMethod;
begin
  if not Hooked then
    Exit;

  Method.Code := FOldCode;
  Method.Data := FOldData;

  SetMethodProp(FObject, FEventName, Method);
  FHooked := False;
end;

end.
 
