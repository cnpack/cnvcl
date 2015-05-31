unit CnTreeTestUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnTree, StdCtrls;

type
  TCnTreeTestForm = class(TForm)
    tvData: TTreeView;
    grpTree: TGroupBox;
    btnLoadFromTreeView: TButton;
    btnSaveToTreeView: TButton;
    btnDepthFirstTravel: TButton;
    btnWidthFirstTravel: TButton;
    grpBTree: TGroupBox;
    btnBLoad: TButton;
    btnBSave: TButton;
    btnPreOrderTravel: TButton;
    btnInOrderTravel: TButton;
    btnPostOrderTravel: TButton;
    btnIsFull: TButton;
    btnIsComplete: TButton;
    btnIsBalance: TButton;
    btnBTreeHeight: TButton;
    btnTreeHeight: TButton;
    grpTrieTree: TGroupBox;
    btnSaveTrie: TButton;
    btnGenerateTrie: TButton;
    btnShowTrieHeight: TButton;
    btnSearch: TButton;
    edtSearch: TEdit;
    chkAnsi: TCheckBox;
    chkCase: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadFromTreeViewClick(Sender: TObject);
    procedure btnSaveToTreeViewClick(Sender: TObject);
    procedure btnDepthFirstTravelClick(Sender: TObject);
    procedure btnWidthFirstTravelClick(Sender: TObject);
    procedure btnBLoadClick(Sender: TObject);
    procedure btnBSaveClick(Sender: TObject);
    procedure btnPreOrderTravelClick(Sender: TObject);
    procedure btnInOrderTravelClick(Sender: TObject);
    procedure btnPostOrderTravelClick(Sender: TObject);
    procedure btnIsFullClick(Sender: TObject);
    procedure btnIsCompleteClick(Sender: TObject);
    procedure btnIsBalanceClick(Sender: TObject);
    procedure btnBTreeHeightClick(Sender: TObject);
    procedure btnTreeHeightClick(Sender: TObject);
    procedure btnGenerateTrieClick(Sender: TObject);
    procedure btnSaveTrieClick(Sender: TObject);
    procedure btnShowTrieHeightClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    { Private declarations }
    FTree: TCnTree;
    FBinaryTree: TCnBinaryTree;
    FTrieTree: TCnTrieTree;
    FTravalResult: string;
    procedure TreeWidthFirstTrav(Sender: TObject);
    procedure TreeDepthFirstTrav(Sender: TObject);
    procedure TreePreOrderTrav(Sender: TObject);
    procedure TreeInOrderTrav(Sender: TObject);
    procedure TreePostOrderTrav(Sender: TObject);
  public
    { Public declarations }
  end;

var
  CnTreeTestForm: TCnTreeTestForm;

implementation

{$R *.DFM}

const
  // 汉语拼音方案，不全，缺 uang 等，不可作拼音参考，仅用来演示字符串插入
  PinYins: array[0..303] of string = ( // ü用 v 代替
    'a',
    'o',
    'e',
    'i',
    'u',
    'v',
    'ai',
    'ei',
    'ui',
    'ao',
    'ou',
    'iu',
    'ie',
    've',
    'er',
    'an',
    'en',
    'in',
    'un',
    'ang',
    'eng',
    'ing',
    'ong',
    'b',
    'ba',
    'bo',
    'bai',
    'bei',
    'bao',
    'ban',
    'ben',
    'bang',
    'beng',
    'bi',
    'bie',
    'biao',
    'bian',
    'bin',
    'bing',
    'p',
    'pa',
    'po',
    'pai',
    'pao',
    'pou',
    'pan',
    'pen',
    'pang',
    'peng',
    'pi',
    'pie',
    'piao',
    'pian',
    'pin',
    'ping',
    'm',
    'ma',
    'mo',
    'me',
    'mai',
    'mao',
    'mou',
    'man',
    'men',
    'mang',
    'meng',
    'mi',
    'mie',
    'miao',
    'miu',
    'mian',
    'min',
    'ming',
    'f',
    'fa',
    'fo',
    'fei',
    'fou',
    'fan',
    'fen',
    'fang',
    'feng',
    'd',
    'da',
    'de',
    'dai',
    'dei',
    'dao',
    'dou',
    'dan',
    'dang',
    'deng',
    'di',
    'die',
    'diao',
    'diu',
    'dian',
    'ding',
    't',
    'ta',
    'te',
    'tai',
    'tao',
    'tou',
    'tan',
    'tang',
    'teng',
    'ti',
    'tie',
    'tiao',
    'tian',
    'ting',
    'n',
    'na',
    'nai',
    'nei',
    'nao',
    'no',
    'nen',
    'nang',
    'neng',
    'ni',
    'nie',
    'niao',
    'niu',
    'nian',
    'nin',
    'niang',
    'ning',
    'l',
    'la',
    'le',
    'lai',
    'lei',
    'lao',
    'lou',
    'lan',
    'lang',
    'leng',
    'li',
    'lia',
    'lie',
    'liao',
    'liu',
    'lian',
    'lin',
    'liang',
    'ling',
    'g',
    'ga',
    'ge',
    'gai',
    'gei',
    'gao',
    'gou',
    'gan',
    'gen',
    'gang',
    'geng',
    'k',
    'ka',
    'ke',
    'kai',
    'kou',
    'kan',
    'ken',
    'kang',
    'keng',
    'h',
    'ha',
    'he',
    'hai',
    'hei',
    'hao',
    'hou',
    'hen',
    'hang',
    'heng',
    'j',
    'ji',
    'jia',
    'jie',
    'jiao',
    'jiu',
    'jian',
    'jin',
    'jiang',
    'jing',
    'q',
    'qi',
    'qia',
    'qie',
    'qiao',
    'qiu',
    'qian',
    'qin',
    'qiang',
    'qing',
    'x',
    'xi',
    'xia',
    'xie',
    'xiao',
    'xiu',
    'xian',
    'xin',
    'xiang',
    'xing',
    'zh',
    'zha',
    'zhe',
    'zhi',
    'zhai',
    'zhao',
    'zhou',
    'zhan',
    'zhen',
    'zhang',
    'zheng',
    'ch',
    'cha',
    'che',
    'chi',
    'chai',
    'chou',
    'chan',
    'chen',
    'chang',
    'cheng',
    'sh',
    'sha',
    'she',
    'shi',
    'shai',
    'shao',
    'shou',
    'shan',
    'shen',
    'shang',
    'sheng',
    'r',
    're',
    'ri',
    'rao',
    'rou',
    'ran',
    'ren',
    'rang',
    'reng',
    'z',
    'za',
    'ze',
    'zi',
    'zai',
    'zao',
    'zou',
    'zang',
    'zeng',
    'c',
    'ca',
    'ce',
    'ci',
    'cai',
    'cao',
    'cou',
    'can',
    'cen',
    'cang',
    'ceng',
    's',
    'sa',
    'se',
    'si',
    'sai',
    'sao',
    'sou',
    'san',
    'sen',
    'sang',
    'seng',
    'y',
    'ya',
    'yao',
    'you',
    'yan',
    'yang',
    'yu',
    'ye',
    'yue',
    'yuan',
    'yi',
    'yin',
    'yun',
    'ying',
    'w',
    'wa',
    'wo',
    'wai',
    'wei',
    'wan',
    'wen',
    'wang',
    'weng',
    'wu'
  );

procedure TCnTreeTestForm.FormCreate(Sender: TObject);
begin
  tvData.FullExpand;
  FTree := TCnTree.Create;
  FTree.OnDepthFirstTravelLeaf := TreeDepthFirstTrav;
  FTree.OnWidthFirstTravelLeaf := TreeWidthFirstTrav;

  FBinaryTree := TCnBinaryTree.Create;
  FBinaryTree.OnPreOrderTravelLeaf := TreePreOrderTrav;
  FBinaryTree.OnInOrderTravelLeaf := TreeInOrderTrav;
  FBinaryTree.OnPostOrderTravelLeaf := TreePostOrderTrav;

  // FTrieTree := TCnTrieTree.Create(False);
end;

procedure TCnTreeTestForm.FormDestroy(Sender: TObject);
begin
  FTrieTree.Free;
  FBinaryTree.Free;
  FTree.Free;
end;

procedure TCnTreeTestForm.btnLoadFromTreeViewClick(Sender: TObject);
begin
  FTree.LoadFromTreeView(tvData);
  ShowMessage('Load OK. Count(Include Root) ' + IntToStr(FTree.Count));
end;

procedure TCnTreeTestForm.btnSaveToTreeViewClick(Sender: TObject);
begin
  if FTree.Count = 1 then
  begin
    ShowMessage('No Content. Do not Save.');
    Exit;
  end;
  FTree.SaveToTreeView(tvData);
  tvData.FullExpand;
  ShowMessage('Save OK. Count ' + IntToStr(tvData.Items.Count));
end;

procedure TCnTreeTestForm.TreeDepthFirstTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreeWidthFirstTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.btnDepthFirstTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FTree.DepthFirstTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnWidthFirstTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FTree.WidthFirstTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnBLoadClick(Sender: TObject);
begin
  FBinaryTree.LoadFromTreeView(tvData);
  FBinaryTree.Root.Text := 'Root';
  ShowMessage('Load OK. Count(Include Root) ' + IntToStr(FBinaryTree.Count));
end;

procedure TCnTreeTestForm.btnBSaveClick(Sender: TObject);
begin
  if FBinaryTree.Count = 1 then
  begin
    ShowMessage('No Content. Do not Save.');
    Exit;
  end;
  FBinaryTree.SaveToTreeView(tvData);
  tvData.FullExpand;
  ShowMessage('Save OK. Count ' + IntToStr(tvData.Items.Count));
end;

procedure TCnTreeTestForm.TreeInOrderTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnBinaryLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreePostOrderTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnBinaryLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreePreOrderTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnBinaryLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.btnPreOrderTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FBinaryTree.PreOrderTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnInOrderTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FBinaryTree.InOrderTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnPostOrderTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FBinaryTree.PostOrderTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnIsFullClick(Sender: TObject);
begin
  if FBinaryTree.IsFull then
    ShowMessage('Full!')
  else
    ShowMessage('NOT Full!');
end;

procedure TCnTreeTestForm.btnIsCompleteClick(Sender: TObject);
begin
  if FBinaryTree.IsComplete then
    ShowMessage('Complete!')
  else
    ShowMessage('NOT Complete!');
end;

procedure TCnTreeTestForm.btnIsBalanceClick(Sender: TObject);
begin
  if FBinaryTree.IsBalance then
    ShowMessage('Balance!')
  else
    ShowMessage('NOT Balance!');
end;

procedure TCnTreeTestForm.btnBTreeHeightClick(Sender: TObject);
begin
  ShowMessage('Tree Height: ' + IntToStr(FBinaryTree.Height));
end;

procedure TCnTreeTestForm.btnTreeHeightClick(Sender: TObject);
begin
  ShowMessage('Tree Height: ' + IntToStr(FTree.Height));
end;

procedure TCnTreeTestForm.btnGenerateTrieClick(Sender: TObject);
var
  I, C, T: Integer;
  Leaf: TCnTrieLeaf;
begin
  FreeAndNil(FTrieTree);
  FTrieTree := TCnTrieTree.Create(chkCase.Checked, False, chkAnsi.Checked);

  C := 0;
  for I := Low(PinYins) to High(PinYins) do
  begin
    Leaf := FTrieTree.InsertString(PinYins[I]);
    if Leaf <> nil then
    begin
      Inc(C);
      Leaf.Data := 1;
    end;
  end;

  T := 0;
  for I := 0 to FTrieTree.Count - 1 do
    if (FTrieTree.Items[I] <> nil) and (FTrieTree.Items[I].Data = 1) then
      Inc(T);

  ShowMessage('Generate OK: ' + IntToStr(C) + ' Check OK: ' + IntToStr(T) + #13#10 +
    'TrieTree Leaf Count(Include Root): ' + IntToStr(FTrieTree.Count));
end;

procedure TCnTreeTestForm.btnSaveTrieClick(Sender: TObject);
begin
  if FTrieTree = nil then
    Exit;

  if FTrieTree.Count = 1 then
  begin
    ShowMessage('No Content. Do not Save.');
    Exit;
  end;
  FTrieTree.SaveToTreeView(tvData);
  tvData.FullExpand;
  ShowMessage('Save OK. Count ' + IntToStr(tvData.Items.Count));
end;

procedure TCnTreeTestForm.btnShowTrieHeightClick(Sender: TObject);
begin
  if FTrieTree = nil then
    Exit;

  ShowMessage('TrieTree Height: ' + IntToStr(FTrieTree.Height));
end;

procedure TCnTreeTestForm.btnSearchClick(Sender: TObject);
var
  Leaf: TCnTrieLeaf;
begin
  if FTrieTree = nil then
    Exit;

  Leaf := FTrieTree.SearchString(edtSearch.Text);
  if Leaf <> nil then
    ShowMessage('Found: ' + Leaf.Text)
  else
    ShowMessage('NOT Found.');
end;

end.
