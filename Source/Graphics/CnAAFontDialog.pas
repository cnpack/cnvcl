{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnAAFontDialog;
{* |<PRE>
================================================================================
* 软件名称：CnPack 控件包
* 单元名称：平滑特效字体对话框控件 TCnAAFontDialog 单元
* 单元作者：CnPack 开发组 周劲羽 (zjy@cnpack.org)
*           移植：e- 
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Build 5/6
* 备　　注：
* 最后更新：2003.03.22
* 移植日期：2006.08.18
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnAAFont, CnAACtrls, ExtCtrls, StdCtrls, ExtDlgs, CnSpin;

type

{ TCnAAFontDlg }

  TCnAAFontDlg = class(TForm)
    Panel1: TPanel;
    gbShadow: TGroupBox;
    cbShadow: TCheckBox;
    Label1: TLabel;
    seShadowBlur: TCnSpinEdit;
    Label2: TLabel;
    seShadowAlpha: TCnSpinEdit;
    Label3: TLabel;
    seOffsetX: TCnSpinEdit;
    Label4: TLabel;
    seOffsetY: TCnSpinEdit;
    spShadow: TShape;
    Label5: TLabel;
    gbGradual: TGroupBox;
    spStartColor: TShape;
    Label10: TLabel;
    cbGradual: TCheckBox;
    spEndColor: TShape;
    Label6: TLabel;
    rbLeftToRight: TRadioButton;
    rbRightToLeft: TRadioButton;
    rbTopToBottom: TRadioButton;
    rbCenterToLR: TRadioButton;
    gbTexture: TGroupBox;
    cbTexture: TCheckBox;
    rbTile: TRadioButton;
    rbStretched: TRadioButton;
    rbCenter: TRadioButton;
    rbNormal: TRadioButton;
    btnOpenPic: TButton;
    gbOther: TGroupBox;
    cbOutline: TCheckBox;
    Label7: TLabel;
    seBlur: TCnSpinEdit;
    Label8: TLabel;
    seAlpha: TCnSpinEdit;
    Label9: TLabel;
    seNoise: TCnSpinEdit;
    btnFont: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    FontDialog: TFontDialog;
    ColorDialog: TColorDialog;
    btnClearPic: TButton;
    Label11: TLabel;
    seSpray: TCnSpinEdit;
    rbBottomToTop: TRadioButton;
    rbCenterToTB: TRadioButton;
    Label12: TLabel;
    seAngle: TCnSpinEdit;
    cbHorzMirror: TCheckBox;
    cbVertMirror: TCheckBox;
    procedure btnFontClick(Sender: TObject);
    procedure cbShadowClick(Sender: TObject);
    procedure seShadowBlurClick(Sender: TObject);
    procedure spShadowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOpenPicClick(Sender: TObject);
    procedure btnClearPicClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FUpdating: Boolean;
    AALabel: TCnAALabel;
    procedure SetEffect(const Value: TCnAAEffect);
    function GetTestFont: TFont;
    procedure SetTestFont(const Value: TFont);
    function GetEffect: TCnAAEffect;
    procedure SetEffectToControls;
    procedure GetEffectFromControls;
    procedure UpdateEffect;
  public
    { Public declarations }
    property Effect: TCnAAEffect read GetEffect write SetEffect;
    property TestFont: TFont read GetTestFont write SetTestFont;
  end;

{ TCnAAFontDialog }

  TCnAAFontDialog = class(TComponent)
  {* 平滑特效字体对话框组件，使用方法同普通对话框}
  private
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FEffect: TCnAAEffect;
    FFont: TFont;
    FAllowChangeFont: Boolean;
    procedure SetEffect(const Value: TCnAAEffect);
    procedure SetTestFont(const Value: TFont);
  protected
    procedure DoShow; virtual;
    procedure DoClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器}
    destructor Destroy; override;
    {* 类析构器}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
    function Execute: Boolean;
    {* 打开对话框，返回执行结果}
  published
    property Effect: TCnAAEffect read FEffect write SetEffect;
    {* 平滑特效参数属性}
    property Font: TFont read FFont write SetTestFont;
    {* 字体属性}
    property AllowChangeFont: Boolean read FAllowChangeFont
      write FAllowChangeFont default False;
    {* 是否允许更改字体，如果为True，将允许用户更改Font属性。如果为False，则不
       显示“字体”按钮。}
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    {* 对话框关闭事件}
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    {* 对话框显示事件}
  end;

implementation

{$R *.DFM}

{ TCnAAFontDlg }

procedure TCnAAFontDlg.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  Panel1.DoubleBuffered := True;
  AALabel := TCnAALabel.Create(Self);
  with AALabel do
  begin
    Parent := Panel1;
    AutoSize := False;
    Align := alClient;
    Font.Charset := GB2312_CHARSET;
    Font.Color := clWindowText;
    Font.Height := -19;
    Font.Name := '楷体_GB2312';
    Font.Style := [];
    Effect.Layout := tlCenter;
    Effect.Alignment := taCenter;
    Effect.BackColor := clWhite;
    Caption := '平滑特效字体 AAFont';
  end;
end;

function TCnAAFontDlg.GetEffect: TCnAAEffect;
begin
  Result := AALabel.Effect.FontEffect;
end;

function TCnAAFontDlg.GetTestFont: TFont;
begin
  Result := AALabel.Font;
end;

procedure TCnAAFontDlg.SetEffect(const Value: TCnAAEffect);
begin
  AALabel.Effect.FontEffect := Value;
  SetEffectToControls;
end;

procedure TCnAAFontDlg.SetTestFont(const Value: TFont);
begin
  AALabel.Font := Value;
end;

procedure TCnAAFontDlg.GetEffectFromControls;
begin
  with AALabel.Effect.FontEffect do
  begin
    Shadow.Enabled := cbShadow.Checked;
    Shadow.Blur := seShadowBlur.Value;
    Shadow.Color := spShadow.Brush.Color;
    Shadow.Alpha := seShadowAlpha.Value;
    Shadow.OffsetX := seOffsetX.Value;
    Shadow.OffsetY := seOffsetY.Value;
    Gradual.Enabled := cbGradual.Checked;
    Gradual.StartColor := spStartColor.Brush.Color;
    Gradual.EndColor := spEndColor.Brush.Color;
    if rbLeftToRight.Checked then
      Gradual.Style := gsLeftToRight
    else if rbRightToLeft.Checked then
      Gradual.Style := gsRightToLeft
    else if rbTopToBottom.Checked then
      Gradual.Style := gsTopToBottom
    else if rbBottomToTop.Checked then
      Gradual.Style := gsBottomToTop
    else if rbCenterToLR.Checked then
      Gradual.Style := gsCenterToLR
    else
      Gradual.Style := gsCenterToTB;
    Texture.Enabled := cbTexture.Checked;
    if rbTile.Checked then
      Texture.Mode := tmTiled
    else if rbStretched.Checked then
      Texture.Mode := tmStretched
    else if rbCenter.Checked then
      Texture.Mode := tmCenter
    else
      Texture.Mode := tmNormal;
    Blur := seBlur.Value;
    Alpha := seAlpha.Value;
    Angle := seAngle.Value;
    Noise := seNoise.Value;
    Spray := seSpray.Value;
    Outline := cbOutline.Checked;
    HorzMirror := cbHorzMirror.Checked;
    VertMirror := cbVertMirror.Checked;
  end;
end;

procedure TCnAAFontDlg.SetEffectToControls;
begin
  FUpdating := True;
  try
    with AALabel.Effect.FontEffect do
    begin
      cbShadow.Checked := Shadow.Enabled;
      seShadowBlur.Value := Shadow.Blur;
      spShadow.Brush.Color := Shadow.Color;
      seShadowAlpha.Value := Shadow.Alpha;
      seOffsetX.Value := Shadow.OffsetX;
      seOffsetY.Value := Shadow.OffsetY;
      cbGradual.Checked := Gradual.Enabled;
      spStartColor.Brush.Color := Gradual.StartColor;
      spEndColor.Brush.Color := Gradual.EndColor;
      rbLeftToRight.Checked := Gradual.Style = gsLeftToRight;
      rbRightToLeft.Checked := Gradual.Style = gsRightToLeft;
      rbTopToBottom.Checked := Gradual.Style = gsTopToBottom;
      rbBottomToTop.Checked := Gradual.Style = gsBottomToTop;
      rbCenterToLR.Checked := Gradual.Style = gsCenterToLR;
      rbCenterToTB.Checked := Gradual.Style = gsCenterToTB;
      cbTexture.Checked := Texture.Enabled;
      rbTile.Checked := Texture.Mode = tmTiled;
      rbStretched.Checked := Texture.Mode = tmStretched;
      rbCenter.Checked := Texture.Mode = tmCenter;
      rbNormal.Checked := Texture.Mode = tmNormal;
      seBlur.Value := Blur;
      seAlpha.Value := Alpha;
      seAngle.Value := Angle;
      seNoise.Value := Noise;
      seSpray.Value := Spray;
      cbOutline.Checked := Outline;
      cbHorzMirror.Checked := HorzMirror;
      cbVertMirror.Checked := VertMirror;
      cbShadowClick(cbShadow);
      cbShadowClick(cbGradual);
      cbShadowClick(cbTexture);
    end;
  finally
    FUpdating := False;
  end;
end;

procedure TCnAAFontDlg.btnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(TestFont);
  if FontDialog.Execute then
    TestFont := FontDialog.Font;
end;

procedure TCnAAFontDlg.cbShadowClick(Sender: TObject);
var
  GroupBox: TGroupBox;
  i: Integer;
begin
  if (Sender is TCheckBox) and (TCheckBox(Sender).Parent is TGroupBox) then
  begin
    GroupBox := TGroupBox(TCheckBox(Sender).Parent);
    for i := 0 to GroupBox.ControlCount - 1 do
      if GroupBox.Controls[i] <> Sender then
        GroupBox.Controls[i].Enabled := TCheckBox(Sender).Checked;
  end;
  UpdateEffect;
end;

procedure TCnAAFontDlg.UpdateEffect;
begin
  if FUpdating then Exit;
  FUpdating := True;
  AALabel.BeginUpdate;
  try
    GetEffectFromControls;
  finally
    AALabel.EndUpdate;
    AALabel.Changed;
    FUpdating := False;
  end;
end;

procedure TCnAAFontDlg.seShadowBlurClick(Sender: TObject);
begin
  UpdateEffect;
end;

procedure TCnAAFontDlg.spShadowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender is TShape) and (Button = mbLeft) then
  begin
    ColorDialog.Color := TShape(Sender).Brush.Color;
    if ColorDialog.Execute then
    begin
      TShape(Sender).Brush.Color := ColorDialog.Color;
      UpdateEffect;
    end;
  end;
end;

procedure TCnAAFontDlg.btnOpenPicClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    AALabel.Effect.FontEffect.Texture.Picture.LoadFromFile(OpenPictureDialog.FileName);
    UpdateEffect;
  end;
end;

procedure TCnAAFontDlg.btnClearPicClick(Sender: TObject);
begin
  AALabel.Effect.FontEffect.Texture.Picture := nil;
  UpdateEffect;
end;

{ TCnAAFontDialog }

procedure TCnAAFontDialog.Assign(Source: TPersistent);
begin
  if Source is TCnAAFontDialog then
  begin
    FFont.Assign(TCnAAFontDialog(Source).FFont);
    FEffect.Assign(TCnAAFontDialog(Source).FEffect);
  end
  else
    inherited;
end;

constructor TCnAAFontDialog.Create(AOwner: TComponent);
begin
  inherited;
  FAllowChangeFont := False;
  FFont := TFont.Create;
  FEffect := TCnAAEffect.Create(nil);
end;

destructor TCnAAFontDialog.Destroy;
begin
  FEffect.Free;
  FFont.Free;
  inherited;
end;

procedure TCnAAFontDialog.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

procedure TCnAAFontDialog.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

function TCnAAFontDialog.Execute: Boolean;
begin
  with TCnAAFontDlg.Create(Self) do
  try
    Effect := Self.FEffect;
    btnFont.Visible := AllowChangeFont;
    if AllowChangeFont then
      TestFont := Self.FFont;
    Self.DoShow;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Self.FEffect.Assign(Effect);
      if AllowChangeFont then
        Self.FFont.Assign(TestFont);
    end;
    Self.DoClose;
  finally
    Free;
  end;
end;

procedure TCnAAFontDialog.SetEffect(const Value: TCnAAEffect);
begin
  FEffect.Assign(Value);
end;

procedure TCnAAFontDialog.SetTestFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

end.



