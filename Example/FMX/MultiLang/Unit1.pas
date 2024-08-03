unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls,  FMX.ExtCtrls, UnitFrame4, FMX.ActnList, FMX.ComboEdit, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.ListView, FMX.Memo, FMX.Menus, FMX.TabControl, FMX.TreeView, FMX.Types, System.Types, System.UITypes,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  System.Actions, FMX.ScrollBox, FMX.Controls.Presentation, CnLangTranslator,
  CnLangMgr, CnClasses, CnLangStorage, CnHashLangStorage;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    lblLangs: TLabel;
    Edit1: TEdit;
    Memo1: TMemo;
    Button1: TButton;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    BitBtn1: TButton;
    ComboBox1: TComboEdit;
    ListBox1: TListBox;
    CheckListBox1: TListBox;
    StatusBar1: TStatusBar;
    PageControl1: TTabControl;
    TabSheet1: TTabItem;
    ListView1: TListView;
    TabSheet2: TTabItem;
    ToolBar1: TGridLayout;
    ToolButton1: TSpeedButton;
    ToolButton2: TSpeedButton;
    ToolButton3: TSpeedButton;
    ToolButton4: TSpeedButton;
    ToolButton5: TSpeedButton;
    TreeView1: TTreeView;
    TTreeViewItem1: TTreeViewItem;
    TTreeViewItem2: TTreeViewItem;
    TTreeViewItem3: TTreeViewItem;
    TTreeViewItem4: TTreeViewItem;
    TTreeViewItem5: TTreeViewItem;
    TTreeViewItem6: TTreeViewItem;
    Button2: TButton;
    btn1: TButton;
    chkStorageMode: TCheckBox;
    mmoLangs: TMemo;
    Button4: TButton;
    Button3: TButton;
    grpFrame: TGroupBox;
    MainMenu1: TMainMenu;
    N1: TMenuITem;
    N3: TMenuITem;
    N4: TMenuITem;
    N2: TMenuITem;
    N5: TMenuITem;
    PopupMenu1: TPopupMenu;
    N6: TMenuITem;
    N7: TMenuITem;
    N8: TMenuITem;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    OpenDialog1: TOpenDialog;
    hfs1: TCnHashLangFileStorage;
    lm1: TCnLangManager;
    lt1: TCnLangTranslator;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure chkStorageModeClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateLangsToMemo;

    procedure LanguageChanged(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

  SCnCurrentLang: string = '当前语言';

  SCnLangTestStrToBeAutoTranslated: string = '这是被Regsiter而将要被自动翻译的字符串';

resourcestring
  SCnLangTestResStrToBeAutoTranslated = '这是被Regsiter而将要被自动翻译的资源字符串';

  SCnLangDetectHint = '重新检测并载入语言后，当前的语言条目以及顺序可能和载入前的不同，'
    + '可能造成当前语言指向的混乱。建议检测载入后重新翻译全部窗体字符串等内容。';

implementation

uses Unit2, Unit3;

{$R *.fmx}

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage(SCnLangDetectHint);
  ShowMessage('Now, Original Path ' + Self.hfs1.LanguagePath);
  ShowMessage('Now, Language Count = ' + IntToStr(hfs1.LanguageCount));

  Self.hfs1.AutoDetect := True;
  Self.hfs1.LanguagePath := '.';

  ShowMessage('After Detection, New Path: ' + hfs1.LanguagePath);
  ShowMessage('After Detection, New Language Count = ' + IntToStr(hfs1.LanguageCount));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnLanguageManager.AddChangeNotifier(LanguageChanged);
  chkStorageMode.IsChecked := hfs1.StorageMode = smByFile;

  RegisterTranslateString(@SCnLangTestStrToBeAutoTranslated, 'SCnLangTestStrToBeAutoTranslated');
  RegisterTranslateResourceString(@SCnLangTestResStrToBeAutoTranslated, 'SCnLangTestResStrToBeAutoTranslated');
  RegisterTranslateResourceString(@SCnLangDetectHint, 'SCnLangDetectHint');

  UpdateLangsToMemo;
end;

procedure TForm1.UpdateLangsToMemo;
var
  I: Integer;
  S: string;
begin
  mmoLangs.Lines.Clear;
  if hfs1.LanguageCount > 0 then
    for I := 0 to hfs1.LanguageCount - 1 do
    begin
      S := Format('%d. %d %s', [I, hfs1.Languages.Items[I].LanguageID,
        hfs1.Languages.Items[I].LanguageName]);
      if I = hfs1.CurrentLanguageIndex then
        S := S + ' ' + SCnCurrentLang;
      mmoLangs.Lines.Add(S);
    end;

  mmoLangs.Lines.Add(SCnLangTestStrToBeAutoTranslated);    
  mmoLangs.Lines.Add(SCnLangTestResStrToBeAutoTranslated);
  // PostMessage(mmoLangs.Handle, WM_KEYDOWN, VK_BACK, 0);
end;

procedure TForm1.chkStorageModeClick(Sender: TObject);
begin
  if chkStorageMode.IsChecked then
    hfs1.StorageMode := smByFile
  else
    hfs1.StorageMode := smByDirectory;

  UpdateLangsToMemo;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  if CnLanguageManager.CurrentLanguageIndex = 0 then
    CnLanguageManager.CurrentLanguageIndex := 1
  else
    CnLanguageManager.CurrentLanguageIndex := 0;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  with TForm2.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Form3 = nil then
    Form3 := TForm3.Create(Application);
    
  Form3.Show;
end;

procedure TForm1.LanguageChanged(Sender: TObject);
begin
  TranslateStr(SCnCurrentLang, 'SCnCurrentLang');
  
  UpdateLangsToMemo;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CnLanguageManager.RemoveChangeNotifier(LanguageChanged);
end;

end.
