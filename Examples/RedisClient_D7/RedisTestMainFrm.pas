unit RedisTestMainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DateUtils, ComCtrls, ExtCtrls, Spin, Menus, CnRedisClient;

type
  TRedisTestFrm = class(TForm)
    mmoResult: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    edtHost: TEdit;
    edtPort: TEdit;
    edtPassword: TEdit;
    btnDisconnect: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    btnKeyScan: TButton;
    btnKeyDel: TButton;
    lblHost: TLabel;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    TabSheet10: TTabSheet;
    TabSheet11: TTabSheet;
    Panel2: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    btnStringSet: TButton;
    btnStringGet: TButton;
    edtStringKey: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtStringValue: TEdit;
    Edit6: TEdit;
    ComboBox1: TComboBox;
    Label3: TLabel;
    SpinEdit1: TSpinEdit;
    Label7: TLabel;
    SpinEdit2: TSpinEdit;
    Label8: TLabel;
    btnKeyDump: TButton;
    btnKeyExists: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    btnKeyMigrate: TButton;
    btnKeyMove: TButton;
    btnKeyObjCount: TButton;
    Button14: TButton;
    btnKeyRename: TButton;
    btnPExpire: TButton;
    btnPExpireat: TButton;
    btnKeyRenameNX: TButton;
    btnKeyRestore: TButton;
    btnKeyPTTL: TButton;
    btnRandomKey: TButton;
    btnKeySort: TButton;
    btnKeyTTL: TButton;
    btnKeyType: TButton;
    Sec_SE: TSpinEdit;
    Label9: TLabel;
    edtDestIP: TEdit;
    edtDestPort: TEdit;
    chkKeyCopy: TCheckBox;
    chkKeyReplace: TCheckBox;
    SpinEdit3: TSpinEdit;
    Label10: TLabel;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
    btnKeyObjCode: TButton;
    btnObjTime: TButton;
    edtKeyParam: TEdit;
    lblKeyParam: TLabel;
    btnStringBitCount: TButton;
    btnStringBitop: TButton;
    btnStringDecr: TButton;
    btnStringDecrBy: TButton;
    btnStringHGetAll: TButton;
    Button34: TButton;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    Label12: TLabel;
    btnSetAdd: TButton;
    Memo2: TMemo;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    lblAMount: TLabel;
    lblLength: TLabel;
    edtKeyText: TEdit;
    lblKey: TLabel;
    edtMatch: TEdit;
    lblMatch: TLabel;
    edtNewKey: TEdit;
    lblNewKey: TLabel;
    edtSetKey: TEdit;
    lblSetKey: TLabel;
    edtSetKey1: TEdit;
    lblSetKey1: TLabel;
    lblSetKey2: TLabel;
    edtSetKey2: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnKeyScanClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnStringSetClick(Sender: TObject);
    procedure btnKeyDelClick(Sender: TObject);
    procedure btnStringGetClick(Sender: TObject);
    procedure btnKeyDumpClick(Sender: TObject);
    procedure btnKeyExistsClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure btnKeyMigrateClick(Sender: TObject);
    procedure N1Click(Sender: TObject);
    procedure btnKeyMoveClick(Sender: TObject);
    procedure btnKeyObjCountClick(Sender: TObject);
    procedure btnKeyObjCodeClick(Sender: TObject);
    procedure btnObjTimeClick(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure btnRandomKeyClick(Sender: TObject);
    procedure btnKeySortClick(Sender: TObject);
    procedure btnKeyTypeClick(Sender: TObject);
    procedure btnStringBitCountClick(Sender: TObject);
    procedure btnKeyRenameNXClick(Sender: TObject);
    procedure btnPExpireatClick(Sender: TObject);
    procedure btnKeyTTLClick(Sender: TObject);
    procedure btnKeyPTTLClick(Sender: TObject);
    procedure btnStringHGetAllClick(Sender: TObject);
    procedure btnKeyRestoreClick(Sender: TObject);
    procedure btnSetAddClick(Sender: TObject);
  private
    { Private declarations }
    FRedisClient: TCnRedisClient;
    procedure SetRedisServer;
    function RandomStr(const sLen: Integer): string;
    function GetGUID: string;
    procedure RedisSADDSet;
  public
    { Public declarations }

  end;

var
  RedisTestFrm: TRedisTestFrm;

implementation

{$R *.dfm}

procedure TRedisTestFrm.Button10Click(Sender: TObject);
begin
  SetRedisServer;
  FRedisClient.KEYS(edtKeyText.Text, mmoResult.Lines);
end;

procedure TRedisTestFrm.btnKeyMigrateClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('MIGRATE:' + BoolToStr(FRedisClient.MIGRATE(edtDestIP.Text,
    StrToInt(edtDestPort.Text), edtKeyText.Text, SpinEdit3.Value, Sec_SE.Value,
    chkKeyCopy.Checked, chkKeyReplace.Checked), True));
end;

procedure TRedisTestFrm.btnKeyMoveClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('MOVE:' + BoolToStr(FRedisClient.MOVE(edtKeyText.Text,
    SpinEdit3.Value), True));
end;

procedure TRedisTestFrm.btnKeyObjCountClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('OBJECTREF_COUNT:' + IntToStr(FRedisClient.OBJECTREFCOUNT(edtKeyText.Text)));
end;

procedure TRedisTestFrm.Button14Click(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('PERSIST:' + BoolToStr(FRedisClient.PERSIST(edtKeyText.Text),
    True));
end;

procedure TRedisTestFrm.btnPExpireatClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('PEXPIREAT:' + booltostr(FRedisClient.PEXPIREAT(edtKeyText.Text,
    DateTimeToUnix(IncSecond(Now, Sec_SE.Value))), True));
end;

procedure TRedisTestFrm.btnKeyRenameNXClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('RENAMENX:' + booltostr(FRedisClient.RENAMENX(edtKeyText.Text,
    edtNewKey.Text), True));
end;

procedure TRedisTestFrm.btnKeyRestoreClick(Sender: TObject);
begin
//FRedisClient.Socket
end;

procedure TRedisTestFrm.btnKeyScanClick(Sender: TObject);
var
  _Value: AnsiString;
  _Ret: TCnRedisMultiBulkNode;
  i, j: Integer;
begin
  SetRedisServer;
  _Ret := FRedisClient.SCAN(SpinEdit1.Value, edtMatch.Text, SpinEdit2.Value);
  mmoResult.Lines.Add(_Ret.Value + ':');
  for i := 0 to _Ret.MultiBulkRefs.Count - 1 do
  begin
    if TCnRedisMultiBulkNode(_Ret.MultiBulkRefs[i]).MultiBulkRefs.Count = 0 then
      mmoResult.Lines.Add(TCnRedisMultiBulkNode(_Ret.MultiBulkRefs[i]).Value)
    else
      for j := 0 to TCnRedisMultiBulkNode(_Ret.MultiBulkRefs[i]).MultiBulkRefs.Count
        - 1 do
        if TCnRedisMultiBulkNode(TCnRedisMultiBulkNode(_Ret.MultiBulkRefs[i]).MultiBulkRefs
          [j]).MultiBulkRefs.Count = 0 then
          mmoResult.Lines.Add(TCnRedisMultiBulkNode(TCnRedisMultiBulkNode(_Ret.MultiBulkRefs
            [i]).MultiBulkRefs[j]).Value);
  end;
  RecycleRedisMultiBulkNode(_Ret);
end;

procedure TRedisTestFrm.btnKeyPTTLClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('PTTL:' + IntToStr(FRedisClient.PTTL(edtKeyText.Text)) + 'ms');
end;

procedure TRedisTestFrm.btnRandomKeyClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('RANDOMKEY:' + FRedisClient.RANDOMKEY);
end;

procedure TRedisTestFrm.btnKeySortClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('SORT:' + IntToStr(FRedisClient.SORT(edtKeyText.Text,
    edtKeyParam.Text, mmoResult.Lines)));
end;

procedure TRedisTestFrm.btnKeyObjCodeClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('OBJECT_ENCODING:' + FRedisClient.OBJECTENCODING(edtKeyText.Text));
end;

procedure TRedisTestFrm.btnKeyTTLClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('TTL:' + IntToStr(FRedisClient.TTL(edtKeyText.Text)) + 's');
end;

procedure TRedisTestFrm.btnKeyTypeClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('TYPE:' + SCN_REDIS_DATA_TYPE_NAME[Integer(FRedisClient._TYPE
    (edtKeyText.Text))]);
end;

procedure TRedisTestFrm.btnObjTimeClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('OBJECT_IDLETIME:' + IntToStr(FRedisClient.OBJECTIDLETIME(edtKeyText.Text)));
end;

procedure TRedisTestFrm.btnStringBitCountClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('BITCOUNT:' + IntToStr(FRedisClient.BITCOUNT(edtKeyText.Text,
    SpinEdit4.Value, SpinEdit5.Value)));
end;

procedure TRedisTestFrm.btnKeyDumpClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('DUMP:' + FRedisClient.DUMP(edtKeyText.Text));
end;

procedure TRedisTestFrm.btnStringHGetAllClick(Sender: TObject);
var
  _Value: TCnRedisKeyValueArray;
begin
  SetRedisServer;
  mmoResult.Lines.Add('HGETALL:' + inttostr(FRedisClient.HGETALL(edtStringKey.Text,
    _Value)));
end;

procedure TRedisTestFrm.btnStringSetClick(Sender: TObject);
begin
  SetRedisServer;
  if FRedisClient._SET(edtStringKey.Text, edtStringValue.Text, StrToInt(Edit6.Text),
    ComboBox1.ItemIndex) then
    mmoResult.Lines.Add('SETKEY OK!')
  else
    mmoResult.Lines.Add('SETKEY ERR!');
end;

procedure TRedisTestFrm.btnSetAddClick(Sender: TObject);
begin
  RedisSADDSet;
end;

procedure TRedisTestFrm.btnDisconnectClick(Sender: TObject);
begin
  FRedisClient.Disconnect;
  mmoResult.Lines.Add('Server is disconnect!');
end;

procedure TRedisTestFrm.btnKeyDelClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('DEL:' + inttostr(FRedisClient.DEL(edtKeyText.Text)));
end;

procedure TRedisTestFrm.btnStringGetClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('GET:' + FRedisClient.GET(edtStringKey.Text));
end;

procedure TRedisTestFrm.btnKeyExistsClick(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('EXISTS:' + booltostr(FRedisClient.EXISTS(edtKeyText.Text), True));
end;

procedure TRedisTestFrm.Button8Click(Sender: TObject);
begin
  SetRedisServer;
  mmoResult.Lines.Add('EXPIRE:' + booltostr(FRedisClient.EXPIRE(edtKeyText.Text,
    Sec_SE.Value), True));
end;

procedure TRedisTestFrm.Button9Click(Sender: TObject);
var
  _Date: TDateTime;
begin
  SetRedisServer;
  _Date := IncSecond(Now, Sec_SE.Value);
  mmoResult.Lines.Add('EXPIREAT:' + DateTimeToStr(_Date) + '==' + booltostr(FRedisClient.EXPIREAT
    (edtKeyText.Text, DateTimeToUnix(_Date)), True));
end;

procedure TRedisTestFrm.FormCreate(Sender: TObject);
begin
  // ReportMemoryLeaksOnShutdown := True;
  FRedisClient := TCnRedisClient.Create;
end;

procedure TRedisTestFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRedisClient);
end;

function TRedisTestFrm.GetGUID: string;
var
  _Str: TGUID;
begin
  CreateGUID(_Str);
  Result := GUIDToString(_Str);
end;

procedure TRedisTestFrm.N1Click(Sender: TObject);
begin
  mmoResult.Lines.Clear;
end;

function TRedisTestFrm.RandomStr(const sLen: Integer): string;
var
  i: Integer;
  s: string;
begin
  Result := '';
  Randomize;
  s := 'Ae6STUnVWXYZopvw897tuHIJg3DEFGqabBCflmrs5cd4hijkKLMN012OPQRxyz';
  for i := 0 to sLen - 1 do
    Result := Result + s[Random(Length(s) - 1) + 1];
end;

procedure TRedisTestFrm.RedisSADDSet;
var
  _Member, _RM: string;
  _Member1, _Member2: string;
  i: Integer;
begin
  _Member := '';
  _Member1 := '';
  _Member2 := '';
  for i := 0 to SpinEdit6.Value - 1 do
  begin
    _RM := RandomStr(SpinEdit7.Value);
    _Member := _Member + _RM + ' ';
    if i < (SpinEdit6.Value div 2) then
      _Member1 := _Member1 + _RM + ' '
    else
      _Member2 := _Member2 + _RM + ' ';

  end;

  SetRedisServer;
  mmoResult.Lines.Add('SADD:' + inttostr(FRedisClient.SADD(edtSetKey.Text, _Member)));
  if edtSetKey1.Text <> '' then
    mmoResult.Lines.Add('SADD1:' + inttostr(FRedisClient.SADD(edtSetKey1.Text,
      _Member1)));
  if edtSetKey2.Text <> '' then
    mmoResult.Lines.Add('SADD2:' + inttostr(FRedisClient.SADD(edtSetKey2.Text,
      _Member2)));

end;

procedure TRedisTestFrm.SetRedisServer;
begin
  mmoResult.Clear;
  FRedisClient.SetRedisServer(edtHost.Text, StrToInt(edtPort.Text), edtPassword.Text);
end;

end.

