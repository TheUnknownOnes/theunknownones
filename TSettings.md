### What is TSettings? ###

TSettings is the name of a bunch of components, which are developed for an easy way to read and write settings out of your application.

This project is currently under development. And this is your chance to get involved with it. Checkout the current release out of the SVN and provide suggestions or patch-files with your ideas.

### The philosophy behind the components ###

The idea behind the components is the following:

  * Create a base component, which holds the settins in a tree structure (TCustomSettings)
  * Provide easy functions (like those one known from TIniFile) to acces the settings (TCustomSettings.GetValue/SetValue)
  * Extend the base component in order to read/write the settings from/to a medium (TSettingsFile, TSettingsStream)
  * Create components which links components to the settings, for automated reading, writing the values of selected properties from/to the settings (TSettingsLinkComponent)
  * Provide a way to cascade settings in order to add missing settings or to override settings by administrative settings

### How to get started ###

  1. Drop a TSettingsXMLFile on your Form
  1. Set the FileName property
  1. If the file exists, load it (SettingsXMLFile1.Load)
  1. Write your settings (SettingsXMLFile.SetValue('/Path/To/Setting', Value))
  1. Save the Settings(SettingsXMLFile.Save)

If yout want to store the form location, try this way:

  1. Drop a TSettingsXMLFile on your form
  1. Drop a TSettingsLinkForm on the form you want to store
  1. Connect the SettingsLink with the Settings
  1. Use TSettings.SaveProperties for defining, which properties of the form you want to save
  1. Rest like the first example