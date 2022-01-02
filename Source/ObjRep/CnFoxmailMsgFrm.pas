{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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

unit CnFoxmailMsgFrm;
{* |<PRE>
================================================================================
* 软件名称：公共窗体库
* 单元名称：仿Foxmail动态提示窗体单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：动态提示窗体是非模态窗体，界面模仿Foxmail V4.0的提示窗口
*           该单元提供以下几个过程用于显示动态提示窗体：
*             ShowMsg     - 显示指定类型的动态窗体
*             ShowInfo    - 显示“提示”类型的动态窗体
*             ShowWarning - 显示“警告”类型的动态窗体
*             ShowError   - 显示“错误”类型的动态窗体
*           通过设置全局变量来指定窗体属性
*             ShowPos     - 指定窗体出现的位置
*             ShowDelay   - 指定窗体显示延时，在窗体上点击左键可强迫其关闭
*           另外，还可通过设置各颜色值指定窗口颜色。
* 使用方法：在需要显示提示窗口的单元中uses本单元，当需要显示提示信息时直接
*           直接调用ShowXXXX过程即可。
* 注意事项：同一时间屏幕上可同时出现多个提示窗体，相互重叠。
*           请勿直接创建窗体实例。
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串尚不符合本地化处理方式
* 修改记录：2002.10.11 V1.1 by Chinbo
*                改用线程定时器定时，修正关闭程序时出错的问题
*           2002.04.03 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnConsts;

type
  TMsgKind = (mkError, mkInfo, mkWarning);
  {* 提示窗口类型
   |<PRE>
     mkError:           - 错误窗口，默认为红色
     mkInfo:            - 提示窗口，默认为蓝色
     mkWarning:         - 警告窗口，默认为黄色
   |</PRE>}

  TShowPos = (spLeft, spRight, spRightTop, spRightBottom);
  {* 提示窗口出现位置
   |<PRE>
     spLeft:            - 窗口由左边移出
     spRight:           - 窗口由右边移出
     spRightTop:        - 窗口由右上角移出
     spRightBottom:     - 窗口由右下角移出
   |</PRE>}

{ TMsgForm }

  TMsgForm = class(TForm)
    lblMsg: TLabel;
    imgWarning: TImage;
    lblIcon: TLabel;
    imgError: TImage;
    imgInfo: TImage;
    procedure FormCreate(Sender: TObject);
    procedure tmFadeInTimer(Sender: TObject);
    procedure tmDelayTimer(Sender: TObject);
    procedure tmFadeOutTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FMsg: string;
    Bmp: TBitmap;
    FMsgKind: TMsgKind;
    StartColor, EndColor: TColor;
    FMsgPos: TShowPos;
    InTimer: Integer;
    InValue: Integer;
    InDraw: Boolean;
    tmFadeIn: TTimer;
    tmFadeOut: TTimer;
    tmDelay: TTimer;
    procedure SetMsg(const Value: string);
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetMsgKind(const Value: TMsgKind);
    procedure SetMsgPos(const Value: TShowPos);
    procedure CreateTimers;
  protected
    function CalcRect(MaxWidth: Integer; const ACap: string; AData: Pointer):
      TRect;
    procedure DrawBk;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    property Msg: string read FMsg write SetMsg;
    property MsgKind: TMsgKind read FMsgKind write SetMsgKind;
    property MsgPos: TShowPos read FMsgPos write SetMsgPos;
  end;

procedure ShowMsg(const Info: string; Kind: TMsgKind = mkInfo); overload;
{* 以非模态方式显示动态提示窗口，参数为提示内容和提示类型}
procedure ShowInfo(const Info: string);
{* 以非模态方式显示提示窗口，参数为提示内容}
procedure ShowWarning(const Info: string);
{* 以非模态方式显示警告窗口，参数为提示内容}
procedure ShowError(const Info: string);
{* 以非模态方式显示错误窗口，参数为提示内容}

var
  ShowPos: TShowPos = spRightBottom;
  {* 提示窗口出现的位置，默认为从右边出现，用户可修改}
  ShowDelay: Integer = 5;
  {* 提示窗口显示延时，默认为从5秒，用户可修改}

  InfoStartColor: TColor = clWhite;
  {* 提示类型窗口起始颜色，默认为白色，用户可修改}
  InfoEndColor: TColor = $00F0E080;
  {* 提示类型窗口结束颜色，默认为浅蓝色，用户可修改}
  WarningStartColor: TColor = clWhite;
  {* 警告类型窗口起始颜色，默认为白色，用户可修改}
  WarningEndColor: TColor = $0080F0E0;
  {* 警告类型窗口起始颜色，默认为浅黄色，用户可修改}
  ErrorStartColor: TColor = clWhite;
  {* 错误类型窗口起始颜色，默认为白色，用户可修改}
  ErrorEndColor: TColor = $008080F0;
  {* 错误类型窗口起始颜色，默认为浅红色，用户可修改}

implementation

{$R *.DFM}

var
  ThisList: TThreadList;

function GetWorkRect: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0)
end;

procedure CleanUp;
var
  iLoop: Integer;
begin
  for iLoop := ThisList.LockList.Count - 1 downto 0 do
    TForm(ThisList.LockList.Items[iLoop]).Free;
end;

procedure ShowMsg(const Info: string; Kind: TMsgKind);
var
  H: Integer;
begin
  with TMsgForm.Create(nil) do
  begin
    Msg := Info;
    MsgKind := Kind;
    MsgPos := ShowPos;
    H := CalcRect(lblMsg.Width, Info, nil).Bottom - lblMsg.Height;
    if H > 0 then
    begin
      lblMsg.Height := lblMsg.Height + H;
      Height := Height + H;
      //没有考虑超过屏幕高度的情形
    end;
    ShowWindow(Handle, SW_SHOWNOACTIVATE); // 窗体不活跃
    SetWindowPos(Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOOWNERZORDER); // 窗体显示在任务栏后面
    FormShow(nil);
  end;
end;

procedure ShowInfo(const Info: string);
begin
  ShowMsg(Info, mkInfo);
end;

procedure ShowWarning(const Info: string);
begin
  ShowMsg(Info, mkWarning);
end;

procedure ShowError(const Info: string);
begin
  ShowMsg(Info, mkError);
end;

{ TMsgForm }

procedure TMsgForm.FormCreate(Sender: TObject);
begin
  Left := -300;
  Top := Screen.Height + 300;
  with ThisList.LockList do
  try
    Add(Self);
  finally
    ThisList.UnlockList;
  end;
  Bmp := TBitmap.Create;
  Bmp.PixelFormat := pf24Bit;
  CreateTimers;
  tmFadeIn.Enabled := True;
end;

procedure TMsgForm.FormShow(Sender: TObject);
begin
  case MsgPos of
    spLeft:
      begin
        Top := (GetWorkRect.Bottom - Height) div 2;
        Left := GetWorkRect.Left + 1 - Width;
      end;
    spRight:
      begin
        Top := (GetWorkRect.Bottom - Height) div 2;
        Left := GetWorkRect.Right - 1;
      end;
    spRightTop:
      begin
        Top := GetWorkRect.Top + 1 - Height;
        Left := GetWorkRect.Right - Width;
      end;
  else
    begin
      Top := GetWorkRect.Bottom - 1;
      Left := GetWorkRect.Right - Width;
    end;
  end;
  tmDelay.Interval := ShowDelay * 1000;
  DrawBk;
end;

procedure TMsgForm.FormDestroy(Sender: TObject);
begin
  Bmp.Free;
  with ThisList.LockList do
  try
    Delete(IndexOf(Self));
  finally
    ThisList.UnlockList;
  end;
end;

procedure TMsgForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  tmFadeIn.Enabled := False;
  tmDelay.Enabled := False;
  tmFadeOut.Enabled := False;
  Action := caFree; // 关闭时释放
end;

procedure TMsgForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_BORDER;
  Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_TOPMOST;
end;

procedure TMsgForm.FormClick(Sender: TObject);
begin
  tmDelayTimer(Self);
end;

procedure TMsgForm.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Bitblt(Msg.DC, 0, 0, ClientWidth, ClientHeight, Bmp.Canvas.Handle, 0, 0,
    SRCCOPY);
  Msg.Result := 1; // 已处理
end;

procedure TMsgForm.tmFadeInTimer(Sender: TObject);
begin
  Inc(InTimer);
  InValue := 1 + InTimer * 3 div 11;
  case MsgPos of
    spLeft:
      begin
        if Left >= GetWorkRect.Left then
        begin
          tmFadeIn.Enabled := False;
          tmDelay.Enabled := True;
        end
        else if Left - InValue >= GetWorkRect.Left then
          Left := GetWorkRect.Left
        else
          Left := Left + InValue;
      end;
    spRight:
      begin
        if Left <= GetWorkRect.Right - Width then
        begin
          tmFadeIn.Enabled := False;
          tmDelay.Enabled := True;
        end
        else if Left + InValue <= GetWorkRect.Right - Width then
          Left := GetWorkRect.Right - Width
        else
          Left := Left - InValue;
      end;
    spRightTop:
      begin
        if Top >= GetWorkRect.Top then
        begin
          tmFadeIn.Enabled := False;
          tmDelay.Enabled := True;
        end
        else if Top + InValue >= GetWorkRect.Top then
          Top := GetWorkRect.Top
        else
          Top := Top + InValue;
      end;
  else
    begin
      if Top + Height <= GetWorkRect.Bottom then
      begin
        tmFadeIn.Enabled := False;
        tmDelay.Enabled := True;
      end
      else if Top - InValue <= GetWorkRect.Bottom - Height then
        Top := GetWorkRect.Bottom - Height
      else
        Top := Top - InValue;
    end;
  end;
end;

procedure TMsgForm.tmDelayTimer(Sender: TObject);
begin
  tmFadeIn.Enabled := False;
  tmDelay.Enabled := False;
  tmFadeOut.Enabled := True;
end;

procedure TMsgForm.tmFadeOutTimer(Sender: TObject);
begin
  if InTimer > 0 then
    Dec(InTimer);
  InValue := 1 + InTimer * 3 div 11;
  case MsgPos of
    spLeft:
      begin
        Left := Left - InValue;
        if Left <= GetWorkRect.Left + 2 - Width then
        begin
          tmFadeOut.Enabled := False;
          Close;
        end;
      end;
    spRight:
      begin
        Left := Left + InValue;
        if Left >= GetWorkRect.Right - 2 then
        begin
          tmFadeOut.Enabled := False;
          Close;
        end;
      end;
    spRightTop:
      begin
        Top := Top - InValue;
        if Top <= GetWorkRect.Top + 1 - Height then
        begin
          tmFadeOut.Enabled := False;
          Close;
        end;
      end;
  else
    begin
      Top := Top + InValue;
      if Top >= GetWorkRect.Bottom - 2 then
      begin
        tmFadeOut.Enabled := False;
        Close;
      end;
    end;
  end;
end;

procedure TMsgForm.SetMsg(const Value: string);
begin
  FMsg := Value;
  lblMsg.Caption := FMsg;
end;

procedure TMsgForm.SetMsgKind(const Value: TMsgKind);
begin
  FMsgKind := Value;
  case FMsgKind of
    mkError:
      begin
        imgError.Visible := True;
        lblIcon.Caption := SCnError;
        StartColor := ColorToRGB(ErrorStartColor);
        EndColor := ColorToRGB(ErrorEndColor);
      end;
    mkWarning:
      begin
        imgWarning.Visible := True;
        lblIcon.Caption := SCnWarning;
        StartColor := ColorToRGB(WarningStartColor);
        EndColor := ColorToRGB(WarningEndColor);
      end;
  else
    begin
      imgInfo.Visible := True;
      lblIcon.Caption := SCnInformation;
      StartColor := ColorToRGB(InfoStartColor);
      EndColor := ColorToRGB(InfoEndColor);
    end;
  end;
end;

procedure TMsgForm.SetMsgPos(const Value: TShowPos);
begin
  FMsgPos := Value;
end;

procedure TMsgForm.CreateTimers;
begin
  tmFadeIn := TTimer.Create(Self);
  with tmFadeIn do
  begin
    Enabled := False;
    Interval := 15;
    OnTimer := tmFadeInTimer;
  end;
  tmFadeOut := TTimer.Create(Self);
  with tmFadeOut do
  begin
    Enabled := False;
    Interval := 15;
    OnTimer := tmFadeOutTimer;
  end;
  tmDelay := TTimer.Create(Self);
  with tmDelay do
  begin
    Enabled := False;
    Interval := 5000;
    OnTimer := tmDelayTimer;
  end;
end;

function TMsgForm.CalcRect(MaxWidth: Integer; const ACap: string;
  AData: Pointer): TRect;
begin
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(lblMsg.Canvas.Handle, PChar(ACap), -1, Result, DT_CALCRECT or DT_LEFT
    or DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
end;

procedure TMsgForm.DrawBk;
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[Byte] of TRGBTriple;
var
  PLine: PRGBArray;
  x, y: Integer;
  ARect: TRect;
  RowInc: Integer;
  sr, sg, sb, er, eg, eb: Integer;
begin
  if InDraw then Exit;
  InDraw := True;
  Bmp.Width := ClientWidth;
  Bmp.Height := ClientHeight;
  sr := GetRValue(StartColor);
  sg := GetGValue(StartColor);
  sb := GetBValue(StartColor);
  er := GetRValue(EndColor);
  eg := GetGValue(EndColor);
  eb := GetBValue(EndColor);
  PLine := PRGBArray(Bmp.ScanLine[0]);
  for x := 0 to Bmp.Width - 1 do
  begin
    PLine[x].rgbtRed := sr + (er - sr) * x div Bmp.Width;
    PLine[x].rgbtGreen := sg + (eg - sg) * x div Bmp.Width;
    PLine[x].rgbtBlue := sb + (eb - sb) * x div Bmp.Width;
  end;
  RowInc := (Bmp.Width * 3 + 3) div 4 * 4;
  for y := 1 to Bmp.Height - 1 do
    Move(PLine^, Bmp.ScanLine[y]^, RowInc);
  ARect := Rect(0, 0, Width, Height);
  Frame3D(Bmp.Canvas, ARect, $777777, $777777, 1);
  InDraw := False;
  Refresh;
end;

initialization
  ThisList := TThreadList.Create;

finalization
  CleanUp;
  FreeAndNil(ThisList);

end.

