# lens-compat
Compatibility shims for converting data-lens code to lens

# How To...

There are several issues that are not addressed by switching to this package.
The primary one is lens composition - with data-lens it is necessary to import
(.) from Control.Category, while lens uses the regular (.) operator from
Prelude.  Furthermore, the order of the lens composition is reversed.  For these
reasons, if any amount of composition is done I recommend the following procedure:

  1. Create a module Compose which imports Control.Category (.) and declares
     (.-.) = flip (.).  Then replace all imports of Control.Category((.)) with imports
     of Compose((.-.)).  This lets us locate all the lens compositions.  Replace uses
     of (.) to compose lenses with uses of (.-.) until everything builds.
     
  2. Change the declaration of (.-.) to flip (.).  Swap all the lens compositions so
     everything builds again.
  
  3. Replace the data-lens dependency with lens and lens-compat, change the declaration
     of .-. to the prelude version of (.).
  
  4. Expand the lens-compat functions inline and remove the lens-compat dependency.
