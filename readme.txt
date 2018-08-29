A simple form data to xml store helper for Delphi.
Copyright (C) 2018 by Alexey Kolesnikov.
Email: ak@blu-disc.net
Website: https://blu-disc.net

You can use this software for whatever you want.


Usage:

  Create, add property names for storing, call Save/Load.
  Optionally you can make some actions before restoring (OnLoad event called after xml loaded, before restoring)
  and before saving (OnSave event called after xml created, before saving). Both methods give you the form root node.

  Property name can be:
    - Simple property name, like ItemIndex, Checked, Value, Text, etc.
    - ControlName|Property, like ComboBox1|Text
    - ClassName.Property, like TMemo.Lines

  You can store properties using three groups:
    - Required group: stores property always if found.
    - Custom group: stores the property by calling the specified procedure (TFormDataXmlStorePropProc).
    - Optional group: stores this property only if nothing has been found in the Required and Custom lists for this control.


Example of usage:

  XmlStore := TFormDataXmlStore.Create(aForm);
  XmlStore.OnLoad := XmlStore_OnLoad; // optional
  XmlStore.OnSave := XmlStore_OnSave; // optional
  XmlStore.AddPropertyRequired('Checked');   // will store Checked for all controls that have it
  XmlStore.AddPropertyRequired('ItemIndex'); // will store ItemIndex for all controls that have it
  XmlStore.AddPropertyRequired('cmbProjectLangCode|Text'); // will store Text for cmbProjectLangCode
  XmlStore.AddPropertyCustom('TMemo.Lines', XmlStoreMemoLines); // will store Lines for all TMemo using XmlStoreMemoLines procedure
  XmlStore.AddPropertyOptional('Text'); // will store Text for all controls that have it, and does not have anything from other lists

  ...

  XmlStore.Save(FileName);

  ...

  XmlStore.Load(FileName);

  ...

  procedure XmlStoreMemoLines(Control: TControl; Node: IXMLNode; XmlStoreCommand: TFormDataXmlStoreCommand);
  begin
    if XmlStoreCommand = apcSave then
      Node.AddChild('Lines').NodeValue := TMemo(Control).Lines.Text
    else
      TMemo(Control).Lines.Text := VarToStr(Node.ChildNodes['Lines'].NodeValue);
  end;

  ...

  procedure TfrmWizard.XmlStore_OnLoad(RootNode: IXMLNode);
  begin
    if RootNode.HasAttribute('ProjectFolder') then
      ProjectFolder := RootNode.Attributes['ProjectFolder'];
  end;

  procedure TfrmWizard.XmlStore_OnSave(RootNode: IXMLNode);
  begin
    RootNode.Attributes['ProjectFolder'] := ProjectFolder;
  end;
