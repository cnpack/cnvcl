unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    btnKey: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    btnKernelMem: TButton;
    lbl1: TLabel;
    lblLen: TLabel;
    edtLen: TEdit;
    UpDown1: TUpDown;
    cbbMem: TComboBox;
    mmoMem: TMemo;
    btnReadPhy: TButton;
    btnChangekernelBase: TButton;
    btnBeep: TButton;
    btnStopBeep: TButton;
    btnCmos: TButton;
    btnReadFirstHardDiskSn: TButton;
    procedure btnKeyClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnKernelMemClick(Sender: TObject);
    procedure btnReadPhyClick(Sender: TObject);
    procedure btnChangekernelBaseClick(Sender: TObject);
    procedure btnBeepClick(Sender: TObject);
    procedure btnStopBeepClick(Sender: TObject);
    procedure btnCmosClick(Sender: TObject);
    procedure btnReadFirstHardDiskSnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  CnSystemDebugControl;

{$R *.DFM}

var
  K: Word = Ord('A');
  SDC: TCnSystemDebugControl;

procedure TForm1.btnKeyClick(Sender: TObject);
begin
  Memo1.SetFocus;
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  SDC.SimuKey(K);
  Inc(K);
  SDC.SimuKey(VK_RETURN);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SDC := TCnSystemDebugControl.Create(Self);
  cbbMem.Text := IntToHex(SDC.KernelBase, 2);
end;

procedure TForm1.btnKernelMemClick(Sender: TObject);
var
  Buf: array[0..100] of AnsiChar;
  Address: Cardinal;
  Len: Integer;
begin
  FillChar(Buf[0], 100, 0);
  Address := StrToInt('$' + cbbMem.Text);
  Len := StrToInt(edtLen.Text);
  SDC.ReadKernelMemory(Address, @Buf[0], Len);

  mmoMem.Text := Buf;
end;

procedure TForm1.btnReadPhyClick(Sender: TObject);
var
  Buf: array[0..100] of AnsiChar;
  Address: Cardinal;
  Len: Integer;
begin
  FillChar(Buf[0], 100, 0);
  Address := StrToInt('$' + cbbMem.Text);
  Len := StrToInt(edtLen.Text);
  SDC.ReadPhysicalMemory(Address, @Buf[0], Len);

  mmoMem.Text := Buf;
end;

procedure TForm1.btnChangekernelBaseClick(Sender: TObject);
var
  Buf: array[0..2] of AnsiChar;
begin
  Buf := 'CH';
  SDC.WriteKernelMemory(SDC.KernelBase, @Buf[0], 2);
  ShowMessage('Write OK. Please Re-read Kernel Memory to Check it.');
end;

procedure TForm1.btnBeepClick(Sender: TObject);
begin
  SDC.BeepOn(1000);
end;

procedure TForm1.btnStopBeepClick(Sender: TObject);
begin
  SDC.BeepOff;
end;

procedure TForm1.btnCmosClick(Sender: TObject);
var
  K, I: Byte;
begin
  mmoMem.Lines.Clear;
  for I := 0 to 255 do
  begin
    K := SDC.ReadCMOS(I);
    mmoMem.Lines.Text := mmoMem.Lines.Text + IntToHex(K, 2) + ' ';
  end;
end;

procedure TForm1.btnReadFirstHardDiskSnClick(Sender: TObject);
begin
  mmoMem.Lines.Text := SDC.ReadFirstHardDiskSerialNumber;
end;

end.
