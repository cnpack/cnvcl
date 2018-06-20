{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnFormScaler;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：在不同的屏幕DPI下，自动调整窗体的字体和大小的组件单元
* 单元作者：Shenloqi, liuzhongwu
* 备    注：
    Scaled = False并不是很好的解决不同分辨率的显示不同的方法，因为这样程序的外观
  跟用户所想看到的不一致，但是如Scaled = True，则如果控件有Align属性，则界面会
  混乱，本控件就是为了解决Scaled = True的时候有Align属性的控件的界面混乱问题的。

    附:发现Delphi的一个问题:Scaled=False,PixelsPerInch=120DPI,AutoScroll=True,
  窗体控件的Align<>[alLeft,alTop]，则在同一DPI下窗体在设计期和运行期不一样，本
  组件可以解决该问题。

* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：
*           2004.11.19 V1.6
*                增加了属性修正当设置Form.Constrains之后，在窗口左上角缩小窗体到
*                  Constrains的最小值之后，会移动窗体位置的BUG(!!!使用了Hook!!!)
*           2004.11.19 V1.5
*                为防止更换了字体，MultiPPI函数不再使用TextHeight计算
*                保存设计期的Width和Height
*                如果有设计期的信息，则可以比较精确的计算出Constraints的大小
*                修正因后设置Constraints而引起的可能窗体变化的情形
*           2004.11.18 V1.4
*                监控动态在窗体创建的控件，并提供方法更新这些控件的大小
*           2004.11.18 V1.3
*                需要改变宽或高时通过重复设定宽或高来防止Delphi自动调整另一个属性
*           2004.11.18 V1.2
*                增强了设计期保存属性的能力，修正一些计算上的小误差
*           2004.11.18 V1.1
*                修正一些BUG
*           2003.06.20 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Math, Forms,
  CnConsts, CnClasses, CnCompConsts;

type
  TCnFormScaler = class(TCnComponent)
  private
    { Private declarations }
    FActive: Boolean;
    FScaled: Boolean;
    FDesignPPI: Integer;
    FScrollForm: Boolean;
    FDesignClientHeight: Integer;
    FDesignClientWidth: Integer;
    FDesignHeight: Integer;
    FDesignWidth: Integer;
    FTextHeight: Integer;
    FControlList: TList;
    FOldWndProc: TWndMethod;
    FFixFormConstrainsResizeBUG: Boolean;
    FForm: TForm;

    function GetDesignClientHeight: Integer;
    function GetDesignClientWidth: Integer;
    function GetDesignHeight: Integer;
    function GetDesignWidth: Integer;
    function GetDesignPPI: Integer;
    function GetTextHeight: Integer;
    procedure SetDesignPPI(const Value: Integer);
    procedure SetDesignClientHeight(const Value: Integer);
    procedure SetDesignClientWidth(const Value: Integer);
    procedure SetDesignHeight(const Value: Integer);
    procedure SetDesignWidth(const Value: Integer);
    procedure SetTextHeight(const Value: Integer);
    procedure SetActive(const Value: boolean);

    procedure ReadDesignPPI(Reader: TReader);
    procedure ReadDesignClientHeight(Reader: TReader);
    procedure ReadDesignClientWidth(Reader: TReader);
    procedure ReadDesignHeight(Reader: TReader);
    procedure ReadDesignWidth(Reader: TReader);
    procedure ReadTextHeight(Reader: TReader);
    procedure WriteDesignPPI(Writer: TWriter);
    procedure WriteDesignClientHeight(Writer: TWriter);
    procedure WriteDesignClientWidth(Writer: TWriter);
    procedure WriteDesignHeight(Writer: TWriter);
    procedure WriteDesignWidth(Writer: TWriter);
    procedure WriteTextHeight(Writer: TWriter);

    procedure DealWMWindowPosChanging(var Message: TMessage);
    procedure FormWndProc(var Message: TMessage);
    procedure HookFormWndProc;
    procedure UnHookFormWndProc;
    procedure SetFixFormConstrainsResizeBUG(const Value: Boolean);
  protected
    { Protected declarations }
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;    
  public
    { Public declarations }
    class function ScreenWorkRect: TRect;
    class function CaptionHeight(const bSmall: Boolean = False): Integer;
    class function NoClientHeight(f: TForm): Integer;
    class function BorderWidth(f: TForm): Integer;
    class procedure UpdateAnchorRules(f: TForm);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDesignTextHeight(frm: TForm): Integer;
    function MultiPPI(const i: Integer; f: TForm): Integer;

    procedure DoEffects;
    procedure ScaleDynamicControls;
  published
    { Published declarations }
    property Active: Boolean read FActive write SetActive default True;

    property DesignPPI: Integer read GetDesignPPI write SetDesignPPI;
    property DesignClientHeight: Integer read GetDesignClientHeight write SetDesignClientHeight;
    property DesignClientWidth: Integer read GetDesignClientWidth write SetDesignClientWidth;
    property DesignHeight: Integer read GetDesignHeight write SetDesignHeight;
    property DesignWidth: Integer read GetDesignWidth write SetDesignWidth;
    property TextHeight: Integer read GetTextHeight write SetTextHeight;

    property Scaled: Boolean read FScaled;
    property ScrollForm: Boolean read FScrollForm write FScrollForm default True;
    property FixFormConstrainsResizeBUG: Boolean
      read FFixFormConstrainsResizeBUG
      write SetFixFormConstrainsResizeBUG
      default False;
  end;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

{ TCnFormScaler }

class function TCnFormScaler.ScreenWorkRect: TRect;
begin
  //Get work area
  {$IFDEF VCL_DOTNET}
  SystemParametersInfo(SPI_GETWORKAREA, 0, Result, 0);
  {$ELSE}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
  {$ENDIF}
end;

class function TCnFormScaler.CaptionHeight(const bSmall: Boolean = False): Integer;
begin
  if bSmall then
    Result := GetSystemMetrics(SM_CYSMCAPTION)
  else
    Result := GetSystemMetrics(SM_CYCAPTION);
end;
(*
var
  ncm: NONCLIENTMETRICS;
begin
  ncm.cbSize := SizeOf(NONCLIENTMETRICS);
  {$IFDEF DELPHI8}
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NONCLIENTMETRICS), ncm, 0);
  {$ELSE}
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NONCLIENTMETRICS), @ncm, 0);
  {$ENDIF}
  if bSmall then
    Result := ncm.iSmCaptionHeight
  else
    Result := ncm.iCaptionHeight;
  if bIncludeBorder then
    Result := Result + ncm.iBorderWidth * 2;
end;
*)

class function TCnFormScaler.NoClientHeight(f: TForm): Integer;
begin
  if Assigned(f) then
    Result := (f.Height - f.ClientHeight)
  else
    Result := CaptionHeight;
end;

class function TCnFormScaler.BorderWidth(f: TForm): Integer;
begin
  if Assigned(f) then
    Result := (f.Width - f.ClientWidth) div 2
  else
    Result := GetSystemMetrics(SM_CXFRAME);
end;

class procedure TCnFormScaler.UpdateAnchorRules(f: TForm);
  procedure DoWithControl(c: TControl);
  var
    OldAnchors: TAnchors;
    i: Integer;
  begin
    with c do
    begin
      for i := 0 to c.ComponentCount - 1 do
        if c.Components[i] is TControl then
          DoWithControl(TControl(c.Components[i]));
      //c.SetBounds(c.Left,c.Top,c.Width,c.Height);
      OldAnchors := Anchors;
      Anchors := [];
      Anchors := [akLeft, akTop, akRight, akBottom];
      Anchors := OldAnchors;
    end;
  end;
var
  i: Integer;
  //OldAnchors: TAnchors;
begin
  //Update all FAnchorRules
  if Assigned(f) then
  begin
    for i := 0 to f.ControlCount - 1 do
      DoWithControl(f.Controls[i]);

    { //应该无需对窗体本身进行处理
    OldAnchors := f.Anchors;
    f.Anchors := [];
    f.Anchors := [akLeft, akTop, akRight, akBottom];
    f.Anchors := OldAnchors;
    }
  end;
end;

function TCnFormScaler.GetDesignTextHeight(frm: TForm): Integer;
var
  NewTH: Integer;
begin
  //Get Design-time TextHeight
  if not Assigned(frm) then
    frm := FForm;
  NewTH := frm.Canvas.TextHeight('0');
  Result := MulDiv(NewTH, FDesignPPI, frm.PixelsPerInch);
end;

function TCnFormScaler.MultiPPI(const i: Integer; f: TForm): Integer;
begin
  //Calc New Size
  if not Assigned(f) then
    f := FForm;

  //GetDesignTextHeight本身就是计算出来的结果,所以这么计算不因字体原因而影响
  Result := MulDiv(i, f.Canvas.TextHeight('0'), GetDesignTextHeight(f));
  { //分类处理虽然计算的快些，但是可能会不够精确
  if f = Owner then
    //使用TextHeight不能正确处理字体变化过的情形
    //Result := MulDiv(i, FForm.Canvas.TextHeight('0'), TextHeight)
    Result := MulDiv(i, f.PixelsPerInch, FDesignPPI)
  else
    //Result := MulDiv(i, f.PixelsPerInch, FDesignPPI);
    //GetDesignTextHeight本身就是计算出来的结果,所以这么计算不因字体原因而影响
    Result := MulDiv(i, f.Canvas.TextHeight('0'), GetDesignTextHeight(f));
  }
end;

constructor TCnFormScaler.Create(AOwner: TComponent);
begin
  //Must on TForm. TFrame not support yet.
{$IFDEF DEBUG}
  CnDebugger.LogMsg('TCnFormScaler Create');
{$ENDIF}
  if not (AOwner is TForm) then
    raise Exception.Create('Owner must inherited from TForm.');

  inherited;

  FForm := TForm(Owner);
  FActive := True;
  FDesignClientHeight := 0;
  FDesignClientWidth := 0;
  FDesignHeight := 0;
  FDesignWidth := 0;
  FDesignPPI := 96;
  FTextHeight := 12;
  FScaled := False;
  FScrollForm := True;
  FFixFormConstrainsResizeBUG := False;
  FOldWndProc := nil;

  FControlList := TList.Create;
end;

destructor TCnFormScaler.Destroy;
begin
  UnHookFormWndProc;

  FControlList.Free;
  inherited;
end;

procedure TCnFormScaler.Loaded;
begin
  //Inplace OnCreate
{$IFDEF DEBUG}
  CnDebugger.LogMsg('TCnFormScaler Loaded');
{$ENDIF}
  inherited Loaded;
  if csDesigning in ComponentState then
  begin
    { //设计期获取这些值没有什么意义
    FDesignPPI := FForm.PixelsPerInch;
    FDesignClientWidth := FForm.ClientWidth;
    FDesignClientHeight := FForm.ClientHeight;
    FDesignWidth := FForm.Width;
    FDesignHeight := FForm.Height;
    FTextHeight := FForm.TextHeight;
    }
  end
  else
    DoEffects;

  //HookFormWndProc;
end;

procedure TCnFormScaler.DoEffects;
var
  PriorHeight, PriorWidth, iCaptionHeight: Integer;
  WorkRect: TRect;
begin
  //Change size
{$IFDEF DEBUG}
  CnDebugger.LogMsg('TCnFormScaler DoEffects');
{$ENDIF}
  if (csDesigning in ComponentState) or
    (not FActive) or
    FScaled or
    (not Assigned(FForm)) then
    Exit;
  WorkRect := ScreenWorkRect;
  with FForm do
  try
    DisableAlign;
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler DisableAlign');
{$ENDIF}
    if AutoScroll and
      (FDesignClientHeight <> 0) and
      (FDesignClientWidth <> 0)  and
      (FDesignHeight <> 0) and
      (FDesignWidth <> 0) then
    begin
      iCaptionHeight := NoClientHeight(FForm);
      //iCaptionHeight := CaptionHeight(BorderStyle in [bsToolWindow, bsSizeToolWin]) + Self.BorderWidth(FForm) * 2;
      {
      MessageBox(0, PChar(
        IntToStr(FDesignClientWidth) + ',' +
        IntToStr(MultiPPI(FDesignClientWidth, FForm)) + ',' +
        IntToStr(MultiPPI(FDesignClientWidth, FForm) + Self.BorderWidth(FForm) * 2) + #13#10 +
        IntToStr(FDesignClientHeight) + ',' +
        IntToStr(MultiPPI(FDesignClientHeight, FForm)) + ',' +
        IntToStr(MultiPPI(FDesignClientHeight, FForm) + iCaptionHeight)// + #13#10 +
        ), '', 0);
      }
      if Scaled then
      begin
        Constraints.MinHeight :=
          Min(MultiPPI(Constraints.MinHeight - (Self.DesignHeight - Self.DesignClientHeight), FForm) + iCaptionHeight,
          WorkRect.Bottom);
        Constraints.MinWidth :=
          Min(MultiPPI(Constraints.MinWidth - (Self.DesignWidth - Self.DesignClientWidth), FForm) + Self.BorderWidth(FForm) * 2,
          WorkRect.Right);
        Constraints.MaxHeight :=
          Min(MultiPPI(Constraints.MaxHeight - (Self.DesignHeight - Self.DesignClientHeight), FForm) + iCaptionHeight,
          WorkRect.Bottom);
        Constraints.MaxWidth :=
          Min(MultiPPI(Constraints.MaxWidth - (Self.DesignWidth - Self.DesignClientWidth), FForm) + Self.BorderWidth(FForm) * 2,
          WorkRect.Right);

        ClientWidth := Min(MultiPPI(FDesignClientWidth, FForm), WorkRect.Right);
        ClientHeight := Min(MultiPPI(FDesignClientHeight, FForm), WorkRect.Bottom - iCaptionHeight);
        //Delphi会自己调整ClientWidth的大小[因为需要Scaled的时，Width变化的时候会引起Height的变化]
        ClientWidth := Min(MultiPPI(FDesignClientWidth, FForm), WorkRect.Right);
        //Width := Min(MultiPPI(FDesignClientWidth, FForm) + Self.BorderWidth(FForm) * 2, WorkRect.Right);
        //Height := Min(MultiPPI(FDesignClientHeight, FForm) + iCaptionHeight, WorkRect.Bottom - iCaptionHeight);
      end
      else
      begin
        Constraints.MinHeight :=
          Min(Constraints.MinHeight - (Self.DesignHeight - Self.DesignClientHeight) + iCaptionHeight,
          WorkRect.Bottom);
        Constraints.MinWidth :=
          Min(Constraints.MinWidth - (Self.DesignWidth - Self.DesignClientWidth) + Self.BorderWidth(FForm) * 2,
          WorkRect.Right);
        Constraints.MaxHeight :=
          Min(Constraints.MaxHeight - (Self.DesignHeight - Self.DesignClientHeight) + iCaptionHeight,
          WorkRect.Bottom);
        Constraints.MaxWidth :=
          Min(Constraints.MaxWidth - (Self.DesignWidth - Self.DesignClientWidth) + Self.BorderWidth(FForm) * 2,
          WorkRect.Right);

        ClientWidth := Min(FDesignClientWidth, WorkRect.Right);
        ClientHeight := Min(FDesignClientHeight, WorkRect.Bottom - iCaptionHeight);
        ClientWidth := Min(FDesignClientWidth, WorkRect.Right);
      end;
    end
    else if Scaled and (BorderStyle in [bsSizeable, bsSizeToolWin]) and AutoScroll then
    begin
      Constraints.MinHeight :=
        Min(MultiPPI(Constraints.MinHeight, FForm), WorkRect.Bottom);
      Constraints.MinWidth :=
        Min(MultiPPI(Constraints.MinWidth, FForm), WorkRect.Right);
      Constraints.MaxHeight :=
        Min(MultiPPI(Constraints.MaxHeight, FForm), WorkRect.Bottom);
      Constraints.MaxWidth :=
        Min(MultiPPI(Constraints.MaxWidth, FForm), WorkRect.Right);

      PriorHeight := Height;
      PriorWidth := Width;
      Width := Min(MultiPPI(PriorWidth, FForm), WorkRect.Right);
      Height := Min(MultiPPI(PriorHeight, FForm), WorkRect.Bottom);
      Width := Min(MultiPPI(PriorWidth, FForm), WorkRect.Right);
    end
    else
    begin
      Constraints.MinHeight := Min(Constraints.MinHeight, WorkRect.Bottom);
      Constraints.MinWidth := Min(Constraints.MinWidth, WorkRect.Right);
      Constraints.MaxHeight := Min(Constraints.MaxHeight, WorkRect.Bottom);
      Constraints.MaxWidth := Min(Constraints.MaxWidth, WorkRect.Right);

      PriorHeight := Height;
      PriorWidth := Width;
      Width := Min(PriorWidth, WorkRect.Right);
      Height := Min(PriorHeight, WorkRect.Bottom);
      Width := Min(PriorWidth, WorkRect.Right);
    end;

    if ScrollForm and
      (not (BorderStyle in [bsSizeable, bsSizeToolWin])) and
      (not AutoScroll) then
      AutoScroll := True;
  finally
    UpdateAnchorRules(FForm);
    EnableAlign;
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler EnableAlign');
{$ENDIF}
    FScaled := True;
  end; //end try and with
end;

procedure TCnFormScaler.SetActive(const Value: boolean);
begin
  //when stored property Active is False, maybe cannot make it.
  FActive := Value;
{$IFDEF DEBUG}
  if Value then
    CnDebugger.LogMsg('TCnFormScaler SetActive: True')
  else
    CnDebugger.LogMsg('TCnFormScaler SetActive: False');
{$ENDIF}
  if (csLoading in ComponentState) then
    Exit;
  DoEffects;
end;

procedure TCnFormScaler.SetDesignPPI(const Value: Integer);
begin
  if csLoading in ComponentState then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler SetDesignPPI ' + IntToStr(Value));
{$ENDIF}
    FDesignPPI := Value;
  end;
end;

procedure TCnFormScaler.SetDesignClientHeight(const Value: Integer);
begin
  if csLoading in ComponentState then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler SetDesignClientHeight ' + IntToStr(Value));
{$ENDIF}
    FDesignClientHeight := Value;
  end;
end;

procedure TCnFormScaler.SetDesignClientWidth(const Value: Integer);
begin
  if csLoading in ComponentState then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler SetDesignClientWidth ' + IntToStr(Value));
{$ENDIF}
    FDesignClientWidth := Value;
  end;
end;

procedure TCnFormScaler.SetDesignHeight(const Value: Integer);
begin
  if csLoading in ComponentState then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler SetDesignHeight ' + IntToStr(Value));
{$ENDIF}
    FDesignHeight := Value;
  end;
end;

procedure TCnFormScaler.SetDesignWidth(const Value: Integer);
begin
  if csLoading in ComponentState then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler SetDesignWidth ' + IntToStr(Value));
{$ENDIF}
    FDesignWidth := Value;
  end;
end;

procedure TCnFormScaler.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('DesignPPI', ReadDesignPPI, WriteDesignPPI, True);
  Filer.DefineProperty('DesignClientHeight', ReadDesignClientHeight, WriteDesignClientHeight, True);
  Filer.DefineProperty('DesignClientWidth', ReadDesignClientWidth, WriteDesignClientWidth, True);
  Filer.DefineProperty('DesignHeight', ReadDesignHeight, WriteDesignHeight, True);
  Filer.DefineProperty('DesignWidth', ReadDesignWidth, WriteDesignWidth, True);
  Filer.DefineProperty('TextHeight', ReadTextHeight, WriteTextHeight, True);
end;

function TCnFormScaler.GetDesignClientHeight: Integer;
begin
  if csDesigning in ComponentState then
    Result := FForm.ClientHeight
  else
    Result := FDesignClientHeight;
end;

function TCnFormScaler.GetDesignClientWidth: Integer;
begin
  if csDesigning in ComponentState then
    Result := FForm.ClientWidth
  else
    Result := FDesignClientWidth;
end;

function TCnFormScaler.GetDesignHeight: Integer;
begin
  if csDesigning in ComponentState then
    Result := FForm.Height
  else
    Result := FDesignHeight;
end;

function TCnFormScaler.GetDesignWidth: Integer;
begin
  if csDesigning in ComponentState then
    Result := FForm.Width
  else
    Result := FDesignWidth;
end;

function TCnFormScaler.GetDesignPPI: Integer;
begin
  if csDesigning in ComponentState then
    Result := FForm.PixelsPerInch
  else
    Result := FDesignPPI;
end;

procedure TCnFormScaler.WriteDesignClientHeight(Writer: TWriter);
begin
  Writer.WriteInteger(GetDesignClientHeight);
end;

procedure TCnFormScaler.WriteDesignClientWidth(Writer: TWriter);
begin
  Writer.WriteInteger(GetDesignClientWidth);
end;

procedure TCnFormScaler.WriteDesignHeight(Writer: TWriter);
begin
  Writer.WriteInteger(GetDesignHeight);
end;

procedure TCnFormScaler.WriteDesignWidth(Writer: TWriter);
begin
  Writer.WriteInteger(GetDesignWidth);
end;

procedure TCnFormScaler.WriteDesignPPI(Writer: TWriter);
begin
  Writer.WriteInteger(GetDesignPPI);
end;

procedure TCnFormScaler.ReadDesignClientHeight(Reader: TReader);
begin
  FDesignClientHeight := Reader.ReadInteger;
end;

procedure TCnFormScaler.ReadDesignClientWidth(Reader: TReader);
begin
  FDesignClientWidth := Reader.ReadInteger;
end;

procedure TCnFormScaler.ReadDesignHeight(Reader: TReader);
begin
  FDesignHeight := Reader.ReadInteger;
end;

procedure TCnFormScaler.ReadDesignWidth(Reader: TReader);
begin
  FDesignWidth := Reader.ReadInteger;
end;

procedure TCnFormScaler.ReadDesignPPI(Reader: TReader);
begin
  FDesignPPI := Reader.ReadInteger;
end;

function TCnFormScaler.GetTextHeight: Integer;
begin
  if csDesigning in ComponentState then
    Result := FForm.Canvas.TextHeight('0')
  else
    Result := FTextHeight;
end;

procedure TCnFormScaler.ReadTextHeight(Reader: TReader);
begin
  FTextHeight := Reader.ReadInteger;
end;

procedure TCnFormScaler.SetTextHeight(const Value: Integer);
begin
  if csLoading in ComponentState then
  begin
{$IFDEF DEBUG}
    CnDebugger.LogMsg('TCnFormScaler SetTextHeight ' + IntToStr(Value)));
{$ENDIF}
    FTextHeight := Value;
  end;
end;

procedure TCnFormScaler.WriteTextHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FForm.Canvas.TextHeight('0'));
end;

procedure TCnFormScaler.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if csReading in ComponentState then
    Exit;
  if Active and FForm.Scaled and (Operation = opInsert) then
  begin
    if AComponent is TControl then
      FControlList.Add(AComponent);
  end;
end;

type
  THackControl = class(TControl);

procedure TCnFormScaler.ScaleDynamicControls;
var
  i: Integer;
  ctrl: TControl;
begin
  if not Active then
    Exit;

  for i := FControlList.Count - 1 downto 0 do
  begin
    if Assigned(FControlList.Items[i]) then
    begin
      ctrl := TControl(FControlList.Items[i]);
      if (ctrl is TCustomForm) or (ctrl is TCustomFrame) then
      begin
        //Do not scale form or frame
      end
      else if ctrl is TWinControl then
      begin
        with TWinControl(ctrl) do
        begin
          ScaleBy(FForm.Canvas.TextHeight('0'), GetDesignTextHeight(FForm));
          //不够精确
          //ScaleBy(FForm.PixelsPerInch, DesignPPI);
          //防止字体发生了变化
          //ScaleBy(FForm.Canvas.TextHeight('0'), TextHeight);
          Left := MultiPPI(Left, nil);
          Top := MultiPPI(Top, nil);
        end;
      end
      else with THackControl(ctrl) do
      begin
        ChangeScale(FForm.Canvas.TextHeight('0'), GetDesignTextHeight(FForm));
        //不够精确
        //ChangeScale(FForm.PixelsPerInch, DesignPPI);
        //防止字体发生了变化
        //ChangeScale(FForm.Canvas.TextHeight('0'), TextHeight);
        Left := MultiPPI(Left, nil);
        Top := MultiPPI(Top, nil);
      end;
      FControlList.Delete(i);
    end;
  end;
end;

procedure TCnFormScaler.FormWndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_WindowPosChanging) and FFixFormConstrainsResizeBUG then
  begin
    DealWMWindowPosChanging(Message);
  end;

  if Assigned(FOldWndProc) then
    FOldWndProc(Message);
end;

procedure TCnFormScaler.HookFormWndProc;
begin
  if not Assigned(FOldWndProc) then
  try
    FOldWndProc := FForm.WindowProc;
    FForm.WindowProc := FormWndProc;
  except
    Application.HandleException(Self);
  end;
end;

procedure TCnFormScaler.UnHookFormWndProc;
begin
  if Assigned(FOldWndProc) then
  try
    FForm.WindowProc := FOldWndProc;
    FOldWndProc := nil;
  except
    Application.HandleException(Self);
  end;
end;

procedure TCnFormScaler.DealWMWindowPosChanging(var Message: TMessage);
var
  aRect: TRect;
  Msg: TWMWindowPosChanging;
begin
  Msg := TWMWindowPosChanging(Message);
  //解决调整边界大小已经到了约束值之后的BUG
  Windows.GetWindowRect(Msg.WindowPos.hwnd, aRect);

  if (Msg.WindowPos.flags and SWP_NOSIZE = 0) then
  begin

    if Msg.WindowPos.cx < FForm.Constraints.MinWidth then
    begin
      Msg.WindowPos.cx := FForm.Constraints.MinWidth;

      if Msg.WindowPos.x <> aRect.Left then
      begin
        Msg.WindowPos.x := aRect.Right - Msg.WindowPos.cx;
      end;
    end
    else if (FForm.Constraints.MaxWidth > 0)
      and (Msg.WindowPos.cx > FForm.Constraints.MaxWidth) then
    begin
      Msg.WindowPos.cx := FForm.Constraints.MaxWidth;
      if Msg.WindowPos.x <> aRect.Left then
      begin
        Msg.WindowPos.x := aRect.Right - Msg.WindowPos.cx;
      end;
    end;

    if Msg.WindowPos.cy < FForm.Constraints.MinHeight then
    begin
      Msg.WindowPos.cy := FForm.Constraints.MinHeight;

      if Msg.WindowPos.y <> aRect.Top then
      begin
        Msg.WindowPos.y := aRect.Bottom - Msg.WindowPos.cy;
      end;
    end
    else if (FForm.Constraints.MaxHeight > 0)
      and (Msg.WindowPos.cy > FForm.Constraints.MaxHeight) then
    begin
      Msg.WindowPos.cy := FForm.Constraints.MaxHeight;
      if Msg.WindowPos.y <> aRect.Top then
      begin
        Msg.WindowPos.y := aRect.Bottom - Msg.WindowPos.cy;
      end;
    end;
  end;
end;

procedure TCnFormScaler.SetFixFormConstrainsResizeBUG(
  const Value: Boolean);
begin
  FFixFormConstrainsResizeBUG := Value;
  if Value then
    HookFormWndProc
  else
    UnHookFormWndProc;
end;

procedure TCnFormScaler.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnFormScalerName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnFormScalerComment;
end;

end.

