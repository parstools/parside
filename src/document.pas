unit document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs, notebook, SynEdit;

type

  { TDocument }

  TDocument = class(TInterfacedObject, IDocument)
  private
    fFileName: string;
    fSheet: TEdSheet;
    fSynEdit: TSynEdit;
    fFactory: IDocumentFactory;
    fHiSyntax: IHiSyntax;
    fUntitledManager: IUntitledManager;
    FUntitledNumber: integer;
  public
    constructor Create(AFactory: IDocumentFactory; ASheet: TEdSheet; ASynEdit: TSynEdit);
    destructor Destroy; override;
    function GetPath: string;
    function GetTitle: string;
    procedure OpenFile(AFileName: string);
    class function CompareFileNames(const S1: string; const S2: string): integer;
    function ComparePathWith(AOtherPath: string): integer;
    procedure Activate;
    function TryClose(): Boolean;
  end;


implementation
uses
  LCLType,SynEditKeyCmds, SynEditStrConst;

{ TDocument }

constructor TDocument.Create(AFactory: IDocumentFactory; ASheet: TEdSheet; ASynEdit: TSynEdit);
var
  CmdIndex: integer;
begin
  fSheet:=ASheet;
  fSheet.IntfPtr := self;
  fSynEdit:=ASynEdit;
  fFactory:=AFactory;
  fHiSyntax:=fFactory.GetHiSyntax;
  fUntitledManager:=fFactory.GetUntitledManager;
  FUntitledNumber:=0;
  with fSynEdit do
  begin
    Options := Options + [eoTrimTrailingSpaces] - [eoSmartTabDelete];
    CmdIndex := Keystrokes.FindCommand(ecInsertLine);
    Keystrokes.Delete(CmdIndex);
    CmdIndex := Keystrokes.FindCommand(ecDeleteWord);
    Keystrokes[CmdIndex].Key:=VK_DELETE;
  end;
end;

destructor TDocument.Destroy;
begin
  fUntitledManager.ReleaseNumber(FUntitledNumber);
  inherited Destroy;
end;

function TDocument.GetPath: string;
begin
  Result := fFileName;
end;

function TDocument.GetTitle: string;
begin
  if fFileName <> '' then
    Result := ExtractFileName(fFileName)
  else
  begin
    if FUntitledNumber=0 then
      fUntitledNumber:=fUntitledManager.GetNewNumber();
    Result := 'Untitled' + IntToStr(fUntitledNumber)
  end;
end;

procedure TDocument.OpenFile(AFileName: string);
begin
  fFileName := AFileName;
  fSheet.Caption := GetTitle;
  if AFileName<>'' then
  begin
    FSynEdit.Lines.LoadFromFile(AFileName);
    FSynEdit.Highlighter := fHiSyntax.GetHighlighterByFileName(AFileName);
  end;
end;

class function TDocument.CompareFileNames(const S1: string; const S2: string): integer;
begin
  if FileNameCaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
end;

function TDocument.ComparePathWith(AOtherPath: string): integer;
begin
  Result := CompareFileNames(fFileName, AOtherPath);
end;

procedure TDocument.Activate;
begin
  fSheet.MakeActive();
end;

function TDocument.TryClose(): Boolean;
begin
  fSheet.TryClose();
end;

end.

