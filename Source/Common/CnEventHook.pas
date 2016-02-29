{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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
* 备    注：该单元用来挂接对象的事件，目前只支持 published 区域的事件以及一些
*               特定事件
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
    FRTTIMethod: Boolean;
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
      新事件处理程序的对象。构造后自动挂接。如 NewData 为 nil，则使用旧 Data}
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

type
  TWinControlHack = class(TWinControl);

  TControlHack = class(TControl);

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
  Name: string;

  procedure PrepareMethodHook;
  begin
    FOldCode := Method.Code;
    FOldData := Method.Data;

    FTrampolineData := TObject(FOldData);
    FTrampoline := FOldCode;

    Method.Code := FNewCode;
    if FNewData = nil then
      Method.Data := FOldData
    else
      Method.Data := FNewData;
  end;

begin
  if Hooked then
    Exit;

  try
    Method := GetMethodProp(FObject, FEventName);
    FRTTIMethod := True;
  except
    FRTTIMethod := False; // No EventName in RTTI
  end;

  if FRTTIMethod then
  begin
    PrepareMethodHook;
    SetMethodProp(FObject, FEventName, Method);

    FHooked := True;
  end
  else // 挨个查
  begin
    Name := UpperCase(FEventName);
    if FObject is TWinControl then
    begin
      if Name = 'ONDOCKDROP' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnDockDrop);
        PrepareMethodHook;
        TWinControlHack(FObject).OnDockDrop := TDockDropEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONDOCKOVER' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnDockOver);
        PrepareMethodHook;
        TWinControlHack(FObject).OnDockOver := TDockOverEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONENTER' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnEnter);
        PrepareMethodHook;
        TWinControlHack(FObject).OnEnter := TNotifyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONEXIT' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnExit);
        PrepareMethodHook;
        TWinControlHack(FObject).OnExit := TNotifyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONGETSITEINFO' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnGetSiteInfo);
        PrepareMethodHook;
        TWinControlHack(FObject).OnGetSiteInfo := TGetSiteInfoEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONKEYDOWN' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnKeyDown);
        PrepareMethodHook;
        TWinControlHack(FObject).OnKeyDown := TKeyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONKEYPRESS' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnKeyPress);
        PrepareMethodHook;
        TWinControlHack(FObject).OnKeyPress := TKeyPressEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONKEYUP' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnKeyUp);
        PrepareMethodHook;
        TWinControlHack(FObject).OnKeyUp := TKeyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONUNDOCK' then
      begin
        Method := TMethod(TWinControlHack(FObject).OnUnDock);
        PrepareMethodHook;
        TWinControlHack(FObject).OnUnDock := TUnDockEvent(Method);
        FHooked := True;
      end;
    end;

    if FObject is TControl then
    begin
      if Name = 'ONCANRESIZE' then
      begin
        Method := TMethod(TControlHack(FObject).OnCanResize);
        PrepareMethodHook;
        TControlHack(FObject).OnCanResize := TCanResizeEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONCLICK' then
      begin
        Method := TMethod(TControlHack(FObject).OnClick);
        PrepareMethodHook;
        TControlHack(FObject).OnClick := TNotifyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONCONSTRAINEDRESIZE' then
      begin
        Method := TMethod(TControlHack(FObject).OnConstrainedResize);
        PrepareMethodHook;
        TControlHack(FObject).OnConstrainedResize := TConstrainedResizeEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONCONTEXTPOPUP' then
      begin
        Method := TMethod(TControlHack(FObject).OnContextPopup);
        PrepareMethodHook;
        TControlHack(FObject).OnContextPopup := TContextPopupEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONDBLCLICK' then
      begin
        Method := TMethod(TControlHack(FObject).OnDblClick);
        PrepareMethodHook;
        TControlHack(FObject).OnDblClick := TNotifyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONDRAGDROP' then
      begin
        Method := TMethod(TControlHack(FObject).OnDragDrop);
        PrepareMethodHook;
        TControlHack(FObject).OnDragDrop := TDragDropEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONDRAGOVER' then
      begin
        Method := TMethod(TControlHack(FObject).OnDragOver);
        PrepareMethodHook;
        TControlHack(FObject).OnDragOver := TDragOverEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONENDDOCK' then
      begin
        Method := TMethod(TControlHack(FObject).OnEndDock);
        PrepareMethodHook;
        TControlHack(FObject).OnEndDock := TEndDragEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONENDDRAG' then
      begin
        Method := TMethod(TControlHack(FObject).OnEndDrag);
        PrepareMethodHook;
        TControlHack(FObject).OnEndDrag := TEndDragEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONMOUSEDOWN' then
      begin
        Method := TMethod(TControlHack(FObject).OnMouseDown);
        PrepareMethodHook;
        TControlHack(FObject).OnMouseDown := TMouseEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONMOUSEMOVE' then
      begin
        Method := TMethod(TControlHack(FObject).OnMouseMove);
        PrepareMethodHook;
        TControlHack(FObject).OnMouseMove := TMouseMoveEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONMOUSEUP' then
      begin
        Method := TMethod(TControlHack(FObject).OnMouseUp);
        PrepareMethodHook;
        TControlHack(FObject).OnMouseUp := TMouseEvent(Method);
        FHooked := True;
      end
{$IFDEF COMPILER6_UP}
      else if Name = 'ONMOUSEWHEEL' then
      begin
        Method := TMethod(TControlHack(FObject).OnMouseWheel);
        PrepareMethodHook;
        TControlHack(FObject).OnMouseWheel := TMouseWheelEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONMOUSEWHEELDOWN' then
      begin
        Method := TMethod(TControlHack(FObject).OnMouseWheelDown);
        PrepareMethodHook;
        TControlHack(FObject).OnMouseWheelDown := TMouseWheelUpDownEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONMOUSEWHEELUP' then
      begin
        Method := TMethod(TControlHack(FObject).OnMouseWheelUp);
        PrepareMethodHook;
        TControlHack(FObject).OnMouseWheelUp := TMouseWheelUpDownEvent(Method);
        FHooked := True;
      end
{$ENDIF}
      else if Name = 'ONRESIZE' then
      begin
        Method := TMethod(TControlHack(FObject).OnResize);
        PrepareMethodHook;
        TControlHack(FObject).OnResize := TNotifyEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONSTARTDOCK' then
      begin
        Method := TMethod(TControlHack(FObject).OnStartDock);
        PrepareMethodHook;
        TControlHack(FObject).OnStartDock := TStartDockEvent(Method);
        FHooked := True;
      end
      else if Name = 'ONSTARTDRAG' then
      begin
        Method := TMethod(TControlHack(FObject).OnStartDrag);
        PrepareMethodHook;
        TControlHack(FObject).OnStartDrag := TStartDragEvent(Method);
        FHooked := True;
      end;
    end;
  end;
end;

procedure TCnEventHook.UnhookEvent;
var
  Method: TMethod;
  Name: string;

  procedure PrepareMethodHook;
  begin
    Method.Code := FOldCode;
    Method.Data := FOldData;
  end;

begin
  if not Hooked then
    Exit;

  if FRTTIMethod then
  begin
    PrepareMethodHook;

    SetMethodProp(FObject, FEventName, Method);
    FHooked := False;
  end
  else
  begin
    Name := UpperCase(FEventName);
    PrepareMethodHook;

    if FObject is TWinControl then
    begin
      if Name = 'ONDOCKDROP' then
      begin
        TWinControlHack(FObject).OnDockDrop := TDockDropEvent(Method);
      end
      else if Name = 'ONDOCKOVER' then
      begin
        TWinControlHack(FObject).OnDockOver := TDockOverEvent(Method);
      end
      else if Name = 'ONENTER' then
      begin
        TWinControlHack(FObject).OnEnter := TNotifyEvent(Method);
      end
      else if Name = 'ONEXIT' then
      begin
        TWinControlHack(FObject).OnExit := TNotifyEvent(Method);
      end
      else if Name = 'ONGETSITEINFO' then
      begin
        TWinControlHack(FObject).OnGetSiteInfo := TGetSiteInfoEvent(Method);
      end
      else if Name = 'ONKEYDOWN' then
      begin
        TWinControlHack(FObject).OnKeyDown := TKeyEvent(Method);
      end
      else if Name = 'ONKEYPRESS' then
      begin
        TWinControlHack(FObject).OnKeyPress := TKeyPressEvent(Method);
      end
      else if Name = 'ONKEYUP' then
      begin
        TWinControlHack(FObject).OnKeyUp := TKeyEvent(Method);
      end
      else if Name = 'ONUNDOCK' then
      begin
        TWinControlHack(FObject).OnUnDock := TUnDockEvent(Method);
      end;
    end;

    if FObject is TControl then
    begin
      if Name = 'ONCANRESIZE' then
      begin
        TControlHack(FObject).OnCanResize := TCanResizeEvent(Method);
      end
      else if Name = 'ONCLICK' then
      begin
        TControlHack(FObject).OnClick := TNotifyEvent(Method);
      end
      else if Name = 'ONCONSTRAINEDRESIZE' then
      begin
        TControlHack(FObject).OnConstrainedResize := TConstrainedResizeEvent(Method);
      end
      else if Name = 'ONCONTEXTPOPUP' then
      begin
        TControlHack(FObject).OnContextPopup := TContextPopupEvent(Method);
      end
      else if Name = 'ONDBLCLICK' then
      begin
        TControlHack(FObject).OnDblClick := TNotifyEvent(Method);
      end
      else if Name = 'ONDRAGDROP' then
      begin
        TControlHack(FObject).OnDragDrop := TDragDropEvent(Method);
      end
      else if Name = 'ONDRAGOVER' then
      begin
        TControlHack(FObject).OnDragOver := TDragOverEvent(Method);
      end
      else if Name = 'ONENDDOCK' then
      begin
        TControlHack(FObject).OnEndDock := TEndDragEvent(Method);
      end
      else if Name = 'ONENDDRAG' then
      begin
        TControlHack(FObject).OnEndDrag := TEndDragEvent(Method);
      end
      else if Name = 'ONMOUSEDOWN' then
      begin
        TControlHack(FObject).OnMouseDown := TMouseEvent(Method);
      end
      else if Name = 'ONMOUSEMOVE' then
      begin
        TControlHack(FObject).OnMouseMove := TMouseMoveEvent(Method);
      end
      else if Name = 'ONMOUSEUP' then
      begin
        TControlHack(FObject).OnMouseUp := TMouseEvent(Method);
      end
{$IFDEF COMPILER6_UP}
      else if Name = 'ONMOUSEWHEEL' then
      begin
        TControlHack(FObject).OnMouseWheel := TMouseWheelEvent(Method);
      end
      else if Name = 'ONMOUSEWHEELDOWN' then
      begin
        TControlHack(FObject).OnMouseWheelDown := TMouseWheelUpDownEvent(Method);
      end
      else if Name = 'ONMOUSEWHEELUP' then
      begin
        TControlHack(FObject).OnMouseWheelUp := TMouseWheelUpDownEvent(Method);
      end
{$ENDIF}
      else if Name = 'ONRESIZE' then
      begin
        TControlHack(FObject).OnResize := TNotifyEvent(Method);
      end
      else if Name = 'ONSTARTDOCK' then
      begin
        TControlHack(FObject).OnStartDock := TStartDockEvent(Method);
      end
      else if Name = 'ONSTARTDRAG' then
      begin
        TControlHack(FObject).OnStartDrag := TStartDragEvent(Method);
      end;
    end;
  end;
end;

end.
 
