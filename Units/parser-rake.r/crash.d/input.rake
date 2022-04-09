if c?(:PO4A_WORKS)
  $available_languages[:manpages].each do |language|
    $manpages.each do |manpage|
      name = manpage.gsub(/man\//, "man/#{language}/")
      file name            => [ name.ext('xml'),     "doc/man/po4a/po/#{language}.po" ]
      file name.ext('xml') => [ manpage.ext('.xml'), "doc/man/po4a/po/#{language}.po" ] do |t|
        runq "po4a", "#{manpage.ext('.xml')} (#{language})", "#{c(:PO4A_TRANSLATE)} #{c(:PO4A_TRANSLATE_FLAGS)} -m #{manpage.ext('.xml')} -p doc/man/po4a/po/#{language}.po -l #{t.name}", :filter_output => $po4a_output_filter
      end
    end
  end
end
# Taken from #3333.
# Sami Farin (@Safari77) made this short test case from mkvtoolnix Rakefile.
