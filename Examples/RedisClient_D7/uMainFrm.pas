unit uMainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Menus, CnRedisClient;

type
  TMainFrm = class(TForm)
    Panel1: TPanel;
    lblHost: TLabel;
    edtHost: TEdit;
    edtPort: TEdit;
    edtPassword: TEdit;
    btnDisconnect: TButton;
    Panel2: TPanel;
    edtStringKey: TLabeledEdit;
    edtStringValue: TLabeledEdit;
    Button1: TButton;
    Button2: TButton;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    Button3: TButton;
    Button4: TButton;
    LabeledEdit5: TLabeledEdit;
    mmoResult: TMemo;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    Splitter1: TSplitter;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FRedisClient:TCnRedisClient;

    procedure SetRedisServer;
  public
    { Public declarations }
  end;

var
  MainFrm: TMainFrm;

implementation

{$R *.dfm}

procedure TMainFrm.Button1Click(Sender: TObject);
begin
  SetRedisServer;
  if FRedisClient._SET(edtStringKey.Text, edtStringValue.Text) then
    mmoResult.Lines.Add('SETKEY OK!')
  else
    mmoResult.Lines.Add('SETKEY ERR!');
end;

procedure TMainFrm.Button2Click(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('GET:' + FRedisClient.GET(edtStringKey.Text));
end;

procedure TMainFrm.Button3Click(Sender: TObject);
begin
  SetRedisServer;
  if FRedisClient.HSET(LabeledEdit3.Text, LabeledEdit4.Text, LabeledEdit5.Text) then
    mmoResult.Lines.Add('HSET OK!')
  else
    mmoResult.Lines.Add('HSET ERR!');
end;

procedure TMainFrm.Button4Click(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('HGET:' + FRedisClient.HGET(LabeledEdit3.Text, LabeledEdit4.Text));
end;

procedure TMainFrm.Button5Click(Sender: TObject);
var
  _RESP,_RESP2:TCnRedisMultiBulk;
  i,j:Integer;
begin
  SetRedisServer;
  _RESP:=FRedisClient.SCAN(100);
  for I := 0 to  _RESP.MultiBulkRefs.Count -2 do
   begin
    _RESP2:= (_RESP.MultiBulkRefs[i]  as TCnRedisMultiBulk);
    if _RESP2.MultiBulkRefs.Count  =0 then  mmoResult.Lines.Add (_RESP2.Value);
    for j := 0 to _RESP2.MultiBulkRefs.Count -2 do
      begin
       mmoResult.Lines.Add (  (_RESP2.MultiBulkRefs[j] as TCnRedisMultiBulk).Value );
      end;
   end;
end;

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  FRedisClient:=TCnRedisClient.Create;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRedisClient);
end;

procedure TMainFrm.N1Click(Sender: TObject);
begin
  mmoResult.Lines.Clear;
end;

procedure TMainFrm.SetRedisServer;
begin
  FRedisClient.SetRedisServer(edtHost.Text, StrToInt(edtPort.Text), edtPassword.Text);
end;

end.
