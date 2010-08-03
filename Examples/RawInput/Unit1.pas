unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    btnStart: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    btnStop: TButton;
    lblKbCount: TLabel;
    btnShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
  private
    { Private declarations }
    procedure RawKeyDown(Sender: TObject; Key: Word; FromKeyBoard: THandle);
    procedure RawKeyUp(Sender: TObject; Key: Word; FromKeyBoard: THandle);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CnRawInput;

{$R *.DFM}

var
  R: TCnRawKeyBoard;

procedure TForm1.FormCreate(Sender: TObject);
begin
  R := TCnRawKeyBoard.Create(Self);
  R.OnRawKeyDown := RawKeyDown;
  R.OnRawKeyUp := RawKeyUp;

  lblKbCount.Caption := lblKbCount.Caption + IntToStr(R.KeyBoardCount);
end;

procedure TForm1.RawKeyDown(Sender: TObject; Key: Word;
  FromKeyBoard: THandle);
begin
  Memo1.Lines.Add(Format('KeyDown: %d, From %d %s', [Key, FromKeyBoard, R.KeyBoardNameFromHandle(FromKeyBoard)]));
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  R.Enabled := True;
end;

procedure TForm1.RawKeyUp(Sender: TObject; Key: Word;
  FromKeyBoard: THandle);
begin
  Memo2.Lines.Add(Format('KeyUp: %d, From %d %s', [Key, FromKeyBoard, R.KeyBoardNameFromHandle(FromKeyBoard)]));
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  R.Enabled := False;
end;

procedure TForm1.btnShowClick(Sender: TObject);
var
  S: string;
  I: Integer;
begin
  for I := 0 to R.KeyBoardCount - 1 do
    S := S + #13#10 + R.KeyBoardName[I];

  ShowMessage(S);
end;

end.
