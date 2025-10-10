{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnAAFontDialog;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �ؼ���
* ��Ԫ���ƣ�ƽ����Ч����Ի���ؼ� TCnAAFontDialog ��Ԫ
* ��Ԫ���ߣ�CnPack ������ �ܾ��� (zjy@cnpack.org)
*           ��ֲ��e- 
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Build 5/6
* ��    ע��
* �����£�2003.03.22
* ��ֲ���ڣ�2006.08.18
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
    property Effect: TCnAAEffect read GetEffect write SetEffect;
    property TestFont: TFont read GetTestFont write SetTestFont;
  end;

{ TCnAAFontDialog }

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnAAFontDialog = class(TComponent)
  {* ƽ����Ч����Ի��������ʹ�÷���ͬ��ͨ�Ի���}
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
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ����}
    function Execute: Boolean;
    {* �򿪶Ի��򣬷���ִ�н��}
  published
    property Effect: TCnAAEffect read FEffect write SetEffect;
    {* ƽ����Ч��������}
    property Font: TFont read FFont write SetTestFont;
    {* ��������}
    property AllowChangeFont: Boolean read FAllowChangeFont
      write FAllowChangeFont default False;
    {* �Ƿ�����������壬���ΪTrue���������û�����Font���ԡ����ΪFalse����
       ��ʾ�����塱��ť��}
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    {* �Ի���ر��¼�}
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    {* �Ի�����ʾ�¼�}
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
    Font.Name := '����_GB2312';
    Font.Style := [];
    Effect.Layout := tlCenter;
    Effect.Alignment := taCenter;
    Effect.BackColor := clWhite;
    Caption := 'ƽ����Ч���� AAFont';
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



