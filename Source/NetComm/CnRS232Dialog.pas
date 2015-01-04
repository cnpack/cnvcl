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

unit CnRS232Dialog;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：CnRS232Dialog 串口设置对话框组件及窗体单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元代码中的字符串符合本地化处理方式
*           该单元窗体中的字符串还不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2002.04.18 V1.1
*                重申明CommConfig和Timeouts为发布属性
*           2002.04.08 V1.0
*                创建单元
*                增加注释
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Buttons, CnClasses, CnConsts, CnRS232, CnNetConsts,
  CnSpin;

type

//------------------------------------------------------------------------------
// 串口设置对话框窗体
//------------------------------------------------------------------------------

{ TCnRS232Dlg }

  TCnRS232Dlg = class(TForm)
    pcCommConfig: TPageControl;
    tsNormal: TTabSheet;
    tsXonXoff: TTabSheet;
    bbtnOk: TBitBtn;
    bbtnCancel: TBitBtn;
    cbbBaudRate: TComboBox;
    lblBaudRate: TLabel;
    cbTxContinueOnXoff: TCheckBox;
    cbOutx_XonXoffFlow: TCheckBox;
    cbInx_XonXoffFlow: TCheckBox;
    lblByteSize: TLabel;
    cbbByteSize: TComboBox;
    lblParity: TLabel;
    cbbParity: TComboBox;
    lblStopBits: TLabel;
    cbbStopBits: TComboBox;
    lblXonLimit: TLabel;
    lblXoffLimit: TLabel;
    lblXonChar: TLabel;
    lblXoffChar: TLabel;
    tsHardware: TTabSheet;
    lblDtrControl: TLabel;
    lblRtsControl: TLabel;
    cbOutx_CtsFlow: TCheckBox;
    cbOutx_DsrFlow: TCheckBox;
    cbDsrSensitivity: TCheckBox;
    cbbDtrControl: TComboBox;
    cbbRtsControl: TComboBox;
    cbReplaceWhenParityError: TCheckBox;
    cbIgnoreNullChar: TCheckBox;
    lblInCtrl: TLabel;
    lblOutCtrl: TLabel;
    tsTimeouts: TTabSheet;
    lblReadIntervalTimeout: TLabel;
    lblReadTotalTimeoutMultiplier: TLabel;
    lblMSec1: TLabel;
    lblMSec2: TLabel;
    lblReadTotalTimeoutConstant: TLabel;
    lblMSec3: TLabel;
    lblWriteTotalTimeoutMultiplier: TLabel;
    lblMSec4: TLabel;
    lblWriteTotalTimeoutConstant: TLabel;
    lblMSec5: TLabel;
    cbShowHint: TCheckBox;
    seReplacedChar: TCnSpinEdit;
    seXonLimit: TCnSpinEdit;
    seXonChar: TCnSpinEdit;
    seXoffChar: TCnSpinEdit;
    seXoffLimit: TCnSpinEdit;
    seReadIntervalTimeout: TCnSpinEdit;
    seReadTotalTimeoutMultiplier: TCnSpinEdit;
    seReadTotalTimeoutConstant: TCnSpinEdit;
    seWriteTotalTimeoutMultiplier: TCnSpinEdit;
    seWriteTotalTimeoutConstant: TCnSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cbbBaudRateExit(Sender: TObject);
    procedure bbtnOkClick(Sender: TObject);
    procedure seReplacedCharExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seXonLimitExit(Sender: TObject);
    procedure ControlChanged(Sender: TObject);
    procedure cbShowHintClick(Sender: TObject);
  private
    { Private declarations }
    FCommConfig: TCnRS232Config;
    FTimeouts: TCnRS232Timeouts;
    procedure SetCommConfig(const Value: TCnRS232Config);
    procedure SetCommTimeouts(const Value: TCnRS232Timeouts);
    procedure ReadCommConfig;
    procedure WriteCommConfig;
    procedure ReadCommTimeouts;
    procedure WriteCommTimeouts;
  public
    { Public declarations }
    property CommConfig: TCnRS232Config read FCommConfig write SetCommConfig;
    property CommTimeouts: TCnRS232Timeouts read FTimeouts write SetCommTimeouts;
  end;

//------------------------------------------------------------------------------
// 串口设置对话框组件
//------------------------------------------------------------------------------

{ TCnRS232Dialog }

  TCnRS232DialogKind = (ckWin32, ckExtended);
  {* 串口设置对话框风格
   |<PRE>
     ckWin32:           - Win32标准风格
     ckExtended:        - 扩展对话框风格
   |</PRE>}
  TCnRS232DialogPages = set of (cpNormal, cpXonXoff, cpHardware, cpTimeouts);
  {* 串口设置对话框显示页面集合
   |<PRE>
     cpNormal:          - 常规设置页面
     cpXonXoff:         - 软件流量控制页面
     cpHardware:        - 硬件流量控制页面
     cpTimeouts:        - 超时设置页面
   |</PRE>}
  TCnRS232DialogShowHint = (csHint, csNoHint, csCheckHint, csCheckNoHint);
  {* 串口设置对话框工具提示信息显示方式
   |<PRE>
     csHint:            - 显示工具提示
     csNoHint:          - 不显示工具提示
     csCheckHint:       - 由单选框决定，默认为显示
     csCheckNoHint:     - 由单选框决定，默认为不显示
   |</PRE>}

  TCnRS232Dialog = class(TCnComponent)
  {* RS232串口设置对话框组件。
   |<PRE>
     * 组件用于显示串口设置对话框，一般搭配TCnRS232组件使用。
     * 使用方式类似于VCL中的常规对话框组件。
   |</PRE>}
  private
    FCommConfig: TCnRS232Config;
    FTimeouts: TCnRS232Timeouts;
    FKind: TCnRS232DialogKind;
    FPages: TCnRS232DialogPages;
    FCommName: string;
    FTitle: string;
    FBaudRateList: Boolean;
    FShowHint: TCnRS232DialogShowHint;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetCommConfig(const Value: TCnRS232Config);
    procedure SetTimeouts(const Value: TCnRS232Timeouts);
    function GetHandle: THandle;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;

    procedure AssignTo(Dest: TPersistent); override;
    procedure DoShow; virtual;
    procedure DoClose; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法，允许从TCnRS232中赋值}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    {* 显示对话框，如果用户点击了“确认”按钮，返回为真}
    property CommName: string read FCommName write FCommName;
    {* 串口端口名}
  published
    property Title: string read FTitle write FTitle;
    {* 对话框标题，仅当 Kind 属性为 ckExtended 扩展风格时有效}
    property Kind: TCnRS232DialogKind read FKind write FKind default ckExtended;
    {* 对话框风格}
    property Pages: TCnRS232DialogPages read FPages write FPages default
      [cpNormal, cpXonXoff, cpHardware];
    {* 对话框可显示的页面集合，仅当 Kind 属性为 ckExtended 扩展风格时有效}
    property BaudRateList: Boolean read FBaudRateList write FBaudRateList default True;
    {* 对话框中的波特率参数是否只允许从下拉列表中选择。如果为假，用户可自定义
       非标准的小特率。仅当 Kind 属性为 ckExtended 扩展风格时有效}
    property ShowHint: TCnRS232DialogShowHint read FShowHint write FShowHint default
      csNoHint;
    {* 显示对话框中工具提示的方式，仅当 Kind 属性为 ckExtended 扩展风格时有效}
    property CommConfig: TCnRS232Config read FCommConfig write SetCommConfig;
    {* 串口通讯设置}
    property Timeouts: TCnRS232Timeouts read FTimeouts write SetTimeouts;
    {* 串口通讯超时设置}
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    {* 对话框关闭事件}
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    {* 对话框显示事件}
  end;

implementation

{$R *.DFM}

//------------------------------------------------------------------------------
// 串口设置对话框窗体
//------------------------------------------------------------------------------

{ TCnRS232Dlg }

// 窗体创建
procedure TCnRS232Dlg.FormCreate(Sender: TObject);
begin
  FCommConfig := TCnRS232Config.Create;
  FTimeouts := TCnRS232Timeouts.Create;
  WriteCommConfig;
  WriteCommTimeouts;
end;

// 窗体释放
procedure TCnRS232Dlg.FormDestroy(Sender: TObject);
begin
  FCommConfig.Free;
  FTimeouts.Free;
end;

// 窗体显示
procedure TCnRS232Dlg.FormShow(Sender: TObject);
begin
  WriteCommConfig;
  WriteCommTimeouts;
  ControlChanged(Self);
end;

// 确定
procedure TCnRS232Dlg.bbtnOkClick(Sender: TObject);
begin
  ReadCommConfig;
  ReadCommTimeouts;
  ModalResult := mrOK;
end;

// 从控件中取串口设置
procedure TCnRS232Dlg.ReadCommConfig;
begin
  with FCommConfig do
  begin
    XoffChar := Char(seXoffChar.Value);
    ReplacedChar := Char(seReplacedChar.Value);
    XonChar := Char(seXonChar.Value);
    Outx_CtsFlow := cbOutx_CtsFlow.Checked;
    Outx_DsrFlow := cbOutx_DsrFlow.Checked;
    ParityCheck := cbbParity.ItemIndex <> 0;
    IgnoreNullChar := cbIgnoreNullChar.Checked;
    Inx_XonXoffFlow := cbInx_XonXoffFlow.Checked;
    TxContinueOnXoff := cbTxContinueOnXoff.Checked;
    ReplaceWhenParityError := cbReplaceWhenParityError.Checked;
    Outx_XonXoffFlow := cbOutx_XonXoffFlow.Checked;
    DsrSensitivity := cbDsrSensitivity.Checked;
    BaudRate := StrToInt(cbbBaudRate.Text);
    ByteSize := TByteSize(cbbByteSize.ItemIndex);
    DtrControl := TDtrControl(cbbDtrControl.ItemIndex);
    Parity := TParity(cbbParity.ItemIndex);
    RtsControl := TRtsControl(cbbRtsControl.ItemIndex);
    StopBits := TStopBits(cbbStopBits.ItemIndex);
    XoffLimit := seXoffLimit.Value;
    XonLimit := seXonLimit.Value;
  end;
end;

// 从控件中取超时设置
procedure TCnRS232Dlg.ReadCommTimeouts;
begin
  with FTimeouts do
  begin
    ReadTotalTimeoutConstant := seReadTotalTimeoutConstant.Value;
    ReadIntervalTimeout := seReadIntervalTimeout.Value;
    ReadTotalTimeoutMultiplier := seReadTotalTimeoutMultiplier.Value;
    WriteTotalTimeoutConstant := seWriteTotalTimeoutConstant.Value;
    WriteTotalTimeoutMultiplier := seWriteTotalTimeoutMultiplier.Value;
  end;
end;

// 根据参数设置控件
procedure TCnRS232Dlg.WriteCommConfig;
begin
  with FCommConfig do
  begin
    seXoffChar.Value := Byte(XoffChar);
    seReplacedChar.Value := Byte(ReplacedChar);
    seXonChar.Value := Byte(XonChar);
    cbOutx_CtsFlow.Checked := Outx_CtsFlow;
    cbOutx_DsrFlow.Checked := Outx_DsrFlow;
    cbIgnoreNullChar.Checked := IgnoreNullChar;
    cbInx_XonXoffFlow.Checked := Inx_XonXoffFlow;
    cbTxContinueOnXoff.Checked := TxContinueOnXoff;
    cbReplaceWhenParityError.Checked := ReplaceWhenParityError;
    cbOutx_XonXoffFlow.Checked := Outx_XonXoffFlow;
    cbDsrSensitivity.Checked := DsrSensitivity;
    if cbbBaudRate.Style = csDropDown then
      cbbBaudRate.Text := IntToStr(BaudRate)
    else
    begin
      cbbBaudRate.ItemIndex := cbbBaudRate.Items.IndexOf(IntToStr(BaudRate));
      if cbbBaudRate.ItemIndex < 0 then
        cbbBaudRate.ItemIndex := cbbBaudRate.Items.Add(IntToStr(BaudRate));
    end;
    cbbByteSize.ItemIndex := Ord(ByteSize);
    cbbDtrControl.ItemIndex := Ord(DtrControl);
    cbbParity.ItemIndex := Ord(Parity);
    cbbRtsControl.ItemIndex := Ord(RtsControl);
    cbbStopBits.ItemIndex := Ord(StopBits);
    seXoffLimit.Value := XoffLimit;
    seXonLimit.Value := XonLimit;
  end;
end;

// 根据超时参数设置控件
procedure TCnRS232Dlg.WriteCommTimeouts;
begin
  with FTimeouts do
  begin
    seReadTotalTimeoutConstant.Value := ReadTotalTimeoutConstant;
    seReadIntervalTimeout.Value := ReadIntervalTimeout;
    seReadTotalTimeoutMultiplier.Value := ReadTotalTimeoutMultiplier;
    seWriteTotalTimeoutConstant.Value := WriteTotalTimeoutConstant;
    seWriteTotalTimeoutMultiplier.Value := WriteTotalTimeoutMultiplier;
  end;
end;

// 设置参数
procedure TCnRS232Dlg.SetCommConfig(const Value: TCnRS232Config);
begin
  FCommConfig.Assign(Value);
  WriteCommConfig;
end;

// 设置超时
procedure TCnRS232Dlg.SetCommTimeouts(const Value: TCnRS232Timeouts);
begin
  FTimeouts.Assign(Value);
  WriteCommTimeouts;
end;

// 约束波特率
procedure TCnRS232Dlg.cbbBaudRateExit(Sender: TObject);
begin
  try
    StrToInt(cbbBaudRate.Text);
  except
    MessageBox(Handle, PChar(SBaudRateError), PChar(SCnError), MB_OK + MB_ICONSTOP);
    cbbBaudRate.SetFocus;
  end;
end;

// 约束字符编辑控件
procedure TCnRS232Dlg.seReplacedCharExit(Sender: TObject);
var
  i: Integer;
begin
  if Sender is TCnSpinEdit then
  try
    i := StrToInt(TCnSpinEdit(Sender).Text);
    if (i > 255) or (i < 0) then
      raise Exception.Create(SCnError);
    if seXonChar.Text = seXoffChar.Text then
    begin
      MessageBox(Handle, PChar(SInvalidXonXoffChar), PChar(SCnError),
        MB_OK + MB_ICONSTOP);
      TCnSpinEdit(Sender).SetFocus;
    end;
  except
    MessageBox(Handle, PChar(SInputASCIICode), PChar(SCnError), MB_OK + MB_ICONSTOP);
    TCnSpinEdit(Sender).SetFocus;
  end;
end;

// 约束整数编辑控件
procedure TCnRS232Dlg.seXonLimitExit(Sender: TObject);
var
  i: Integer;
begin
  if Sender is TCnSpinEdit then
  try
    i := StrToInt(TCnSpinEdit(Sender).Text);
    if (i > MaxWord) or (i < 0) then
      raise Exception.Create(SCnError);
  except
    MessageBox(Handle, PChar(SInputInteger), PChar(SCnError), MB_OK + MB_ICONSTOP);
    TCnSpinEdit(Sender).SetFocus;
  end;
end;

// 设置控件状态
procedure TCnRS232Dlg.ControlChanged(Sender: TObject);
begin
  cbReplaceWhenParityError.Enabled := cbbParity.ItemIndex > 0;
  seReplacedChar.Enabled := cbReplaceWhenParityError.Enabled and
    cbReplaceWhenParityError.Checked;
end;

// 设置工具提示
procedure TCnRS232Dlg.cbShowHintClick(Sender: TObject);
begin
  ShowHint := cbShowHint.Checked;
end;

//------------------------------------------------------------------------------
// 串口设置对话框组件
//------------------------------------------------------------------------------

{ TCnRS232Dialog }

// 对象赋值方法
procedure TCnRS232Dialog.Assign(Source: TPersistent);
begin
  if Source is TCnRS232 then
  begin
    FCommConfig.Assign(TCnRS232(Source).CommConfig);
    FTimeouts.Assign(TCnRS232(Source).Timeouts);
    FCommName := TCnRS232(Source).CommName;
  end
  else if Source is TCnRS232Dialog then
  begin
    TCnRS232Dialog(Source).AssignTo(Self);
  end
  else
    inherited;
end;

// 目标对象赋值方法
procedure TCnRS232Dialog.AssignTo(Dest: TPersistent);
begin
  if Dest is TCnRS232 then
  begin
    TCnRS232(Dest).CommConfig := FCommConfig;
    TCnRS232(Dest).Timeouts := FTimeouts;
  end
  else if Dest is TCnRS232Dialog then
  begin
    TCnRS232Dialog(Dest).FCommConfig.Assign(FCommConfig);
    TCnRS232Dialog(Dest).FTimeouts.Assign(FTimeouts);
    TCnRS232Dialog(Dest).FCommName := FCommName;
  end
  else
    inherited;
end;

// 初始化
constructor TCnRS232Dialog.Create(AOwner: TComponent);
begin
  inherited;
  FCommConfig := TCnRS232Config.Create;
  FTimeouts := TCnRS232Timeouts.Create;
  FKind := ckExtended;
  FPages := [cpNormal, cpXonXoff, cpHardware];
  FBaudRateList := True;
  FShowHint := csNoHint;
end;

// 释放
destructor TCnRS232Dialog.Destroy;
begin
  FCommConfig.Free;
  FTimeouts.Free;
  inherited;
end;

// 对话框关闭
procedure TCnRS232Dialog.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(Self);
end;

// 对话框显示
procedure TCnRS232Dialog.DoShow;
begin
  if Assigned(FOnShow) then
    FOnShow(Self);
end;

// 显示对话框
function TCnRS232Dialog.Execute: Boolean;
var
  CnRS232Dlg: TCnRS232Dlg;
  lpCC: TCommConfig;
begin
  if FKind = ckExtended then  // 扩展风格
  begin
    CnRS232Dlg := TCnRS232Dlg.Create(Owner);
    try
      CnRS232Dlg.FCommConfig.Assign(FCommConfig);
      CnRS232Dlg.FTimeouts.Assign(FTimeouts);
      if FTitle <> '' then
        CnRS232Dlg.Caption := FTitle
      else if FCommName <> '' then
        CnRS232Dlg.Caption := Format('%s (%s)', [CnRS232Dlg.Caption, FCommName]);
      if FBaudRateList then
        CnRS232Dlg.cbbBaudRate.Style := csDropDownList
      else
        CnRS232Dlg.cbbBaudRate.Style := csDropDown;
      CnRS232Dlg.cbShowHint.Visible := FShowHint in [csCheckHint, csCheckNoHint];
      CnRS232Dlg.cbShowHint.Checked := FShowHint in [csHint, csCheckHint];
      CnRS232Dlg.ShowHint := CnRS232Dlg.cbShowHint.Checked;
      CnRS232Dlg.tsNormal.TabVisible := cpNormal in FPages;
      CnRS232Dlg.tsXonXoff.TabVisible := cpXonXoff in FPages;
      CnRS232Dlg.tsHardware.TabVisible := cpHardware in FPages;
      CnRS232Dlg.tsTimeouts.TabVisible := cpTimeouts in FPages;
      if FPages = [] then
        CnRS232Dlg.tsNormal.TabVisible := True;
      DoShow;
      Result := CnRS232Dlg.ShowModal = mrOK;
      if Result then
      begin
        FCommConfig.Assign(CnRS232Dlg.FCommConfig);
        FTimeouts.Assign(CnRS232Dlg.FTimeouts);
      end;
      DoClose;
    finally
      CnRS232Dlg.Free;
    end;
  end
  else
  begin
    FillChar(lpCC, SizeOf(lpCC), 0);
    lpCC.dwSize := SizeOf(lpCC);
    FCommConfig.GetDCB(lpCC.DCB);
    DoShow;
    Result := CommConfigDialog(PChar(FCommName), GetHandle, lpCC);
    if Result then
      FCommConfig.SetDCB(lpCC.DCB);
    DoClose;
  end;
end;

procedure TCnRS232Dialog.GetComponentInfo(var AName, Author, Email, Comment: string);
begin
  AName := SCnRS232DialogName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnRS232DialogComment;
end;

// 取父窗体句柄
function TCnRS232Dialog.GetHandle: THandle;
begin
  if Owner is TForm then
    Result := TForm(Owner).Handle
  else
    Result := 0;
end;

// 设置参数
procedure TCnRS232Dialog.SetCommConfig(const Value: TCnRS232Config);
begin
  FCommConfig.Assign(Value);
end;

// 设置超时
procedure TCnRS232Dialog.SetTimeouts(const Value: TCnRS232Timeouts);
begin
  FTimeouts.Assign(Value);
end;

end.

