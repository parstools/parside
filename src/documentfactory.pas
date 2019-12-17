unit documentfactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs, notebook;

type

  { TDocumentFactory }

  TDocumentFactory = class(TInterfacedObject, IDocumentFactory)
  private
    fNotebook: TEdNotebook;
    fUntitledManager: IUntitledManager;
  public
    constructor Create(ANotebook: TEdNotebook);
    function GetDocumentCount: integer;
    function GetDocument(Index: integer): IDocument;
    function CreateNew(AFileName: string): IDocument;
    function GetActive: IDocument;
  end;

implementation
uses
  Controls, Graphics, SynEdit, Document, untitledmanager;

{ TDocumentFactory }

constructor TDocumentFactory.Create(ANotebook: TEdNotebook);
begin
  fNotebook := ANotebook;
  fUntitledManager := TUntitledManager.Create();
end;

function TDocumentFactory.GetDocumentCount: integer;
begin
  Result := fNotebook.PageCount;
end;

function TDocumentFactory.GetDocument(Index: integer): IDocument;
begin
  Result := fNotebook.Page[Index].IntfPtr as IDocument;
end;

function TDocumentFactory.CreateNew(AFileName: string): IDocument;
var
  sheet: TEdSheet;
  syn: TSynEdit;
begin
  sheet := FNotebook.AddTabSheet();
  syn := TSynEdit.Create(sheet);
  syn.Color := clWhite;
  syn.BorderStyle := bsNone;
  syn.Align := alClient;
  sheet.InsertControl(syn);
  sheet.FocusedControl := syn;
  Result := TDocument.Create(sheet, syn, fUntitledManager);
end;

function TDocumentFactory.GetActive: IDocument;
begin
  if FNotebook.ActiveSheet=nil then
     Result := nil
  else
     Result := FNotebook.ActiveSheet.IntfPtr as IDocument;
end;

end.
