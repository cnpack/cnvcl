unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnIocpSimpleMemPool, StdCtrls;

type
  TForm1 = class(TForm)
    cncpsmplmpl1: TCnIocpSimpleMemPool;
    btn1: TButton;
    btnFree: TButton;
    btnDisp: TButton;
    mmo1: TMemo;
    procedure btn1Click(Sender: TObject);
    procedure btnDispClick(Sender: TObject);
    procedure btnFreeClick(Sender: TObject);
  private
    { Private declarations }
    Rented: Boolean;
    FtestBuffer: PAnsiChar;
    FBufArr :array[0..100] of PAnsiChar;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
const
  buffSize = 50;
var
  i: Integer;
begin
  if Rented then 
    Exit;
    
  try
    cncpsmplmpl1.RentMemory(Pointer(FtestBuffer));
    for i := 0 to 10 do
      FtestBuffer[i] := Chr(i);

    for i := 0 to 100 do
      cncpsmplmpl1.RentMemory(Pointer(FBufArr[i]));

    Rented := True;
    ShowMessage('Memory Rented');
  except 
    on ee: Exception do
      ShowMessage(ee.Message);
  end;
end;

procedure TForm1.btnDispClick(Sender: TObject);
var
  i: integer;
begin
  mmo1.Lines.Clear;
  if Rented then
    for i := 0 to 100 do
      mmo1.Lines.Add(IntToHex(integer(FBufArr[i]), 2));
end;

procedure TForm1.btnFreeClick(Sender: TObject);
var
  i:integer;
begin
  if not Rented then 
    Exit;
    
  try
    cncpsmplmpl1.ReturnMemory(Pointer(FtestBuffer));

    for i := 0 to 100 do
      cncpsmplmpl1.ReturnMemory(Pointer(FBufArr[i]));

    Rented := False;
    ShowMessage('Memory Returned.');
  except
    on ee: Exception do
      ShowMessage(ee.Message);
  end;
end;

end.
