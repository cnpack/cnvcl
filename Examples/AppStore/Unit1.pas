unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, CnFMAppStore;

type
  TForm1 = class(TForm)
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ExtItemClick(Sender: TObject; AData: Pointer);
    procedure ExtButtonClick(Sender: TObject; AData: Pointer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ExtButtonClick(Sender: TObject; AData: Pointer);
begin
  //
  ShowMessage('Button Clicked');
end;

procedure TForm1.ExtItemClick(Sender: TObject; AData: Pointer);
begin
  //
  ShowMessage('Item Double Clicked');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  cnt: integer;
begin
  cnt := 0;
  for i:=1 to 10 do
  begin
    Application.ProcessMessages;
    with TCnFMAppStore.Create(ScrollBox1) do
    begin
      Parent := ScrollBox1;
      Top := cnt * 46+1;
      Align := alTop;
      Data := nil;
      AppSize := '1.0M';
      AppName := 'Test App '+ IntToStr(i);
      AppDesc := AppName;
      Status := asNotInstalled;
      Image.Picture.Bitmap.LoadFromFile('Img\Head.bmp');
      Image.Stretch := True;
      OnItemDblClick := ExtItemClick;
      OnButtonClick := ExtButtonClick;
      inc(cnt);
    end;
  end;
end;

end.
