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
  private
    { Private declarations }
    FTree: TCnTree;
    FBinaryTree: TCnBinaryTree;
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
end;

procedure TCnTreeTestForm.FormDestroy(Sender: TObject);
begin
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

end.
