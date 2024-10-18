**Vst3Ada**

A Pure Ada Implementation of the VST3 API.

WIP.

**Resources:**

* Library Building:
    - https://www.adacore.com/gems/gem-109-ada-plug-ins-and-shared-libraries-part-1
    - https://www.adacore.com/gems/gem-110-ada-plug-ins-and-shared-libraries-part-2
    - https://gcc.gnu.org/onlinedocs/gcc-4.7.4/gnat_ugn_unw/Building-a-library.html
    - https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html#h3.15
    - https://gcc.gnu.org/onlinedocs/gnat_ugn/Building-a-Stand-alone-Library.html

* Inspiring Ada Projects:
    - https://github.com/tsoding/eepers
    - https://www.youtube.com/watch?v=MUISz2qA640 (Couldn't have actually done this without this video).

* Debugging Tools:
    - On Linux, Debugging was pretty difficult but VS Code and JUCE were very helpful:
    - https://github.com/juce-framework/JUCE
    - JUCE is pretty relaxed about what it'll try load and you can step through the
      loading process to see what's going on.

* Inspiring Audio Projects:
    - https://github.com/Tremus/CPLUG/tree/master CPLUG is great and has a really
      simple implementation of the VST3 API.
    - https://github.com/RustAudio/vst3-sys/blob/master/src/vst/ivstaudioprocessor.rs#L46
      someone over at rust has also done some bindings, these are a good reference point for
      what else needs doing.
