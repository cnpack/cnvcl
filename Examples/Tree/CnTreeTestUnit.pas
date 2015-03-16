unit CnTreeTestUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnTree, StdCtrls;

type
  TCnTreeTestForm = class(TForm)
    tvData: TTreeView;
    btnLoadFromTreeView: TButton;
    btnSaveToTreeView: TButton;
    btnDepthFirstTraval: TButton;
    btnWidthFirstTraval: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadFromTreeViewClick(Sender: TObject);
    procedure btnSaveToTreeViewClick(Sender: TObject);
    procedure btnDepthFirstTravalClick(Sender: TObject);
    procedure btnWidthFirstTravalClick(Sender: TObject);
  private
    { Private declarations }
    FTree: TCnTree;
    FTravalResult: string;
    procedure TreeWidthFirstTrav(Sender: TObject);
    procedure TreeDepthFirstTrav(Sender: TObject);
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
end;

procedure TCnTreeTestForm.FormDestroy(Sender: TObject);
begin
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

procedure TCnTreeTestForm.btnDepthFirstTravalClick(Sender: TObject);
begin
  FTravalResult := '';
  FTree.DepthFirstTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnWidthFirstTravalClick(Sender: TObject);
begin
  FTravalResult := '';
  FTree.WidthFirstTravel;
  ShowMessage(FTravalResult);
end;

end.
