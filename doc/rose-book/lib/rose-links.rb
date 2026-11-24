# Links for the Rose Book's references to ADRs and issues.
#
# The chapters write "ADR 0044", "#1096" and "idle#44" as plain text.
# This include processor reads each included chapter (a .adoc file) and
# makes each of those a link: an ADR to its file in doc/adr on GitHub, an
# issue or a pull request to its page (GitHub sends an issue's number to
# the pull request when it is one), and another repository's issue to
# that repository.  Other includes, such as the source files that a
# chapter shows, are left to Asciidoctor, unchanged.
#
# Nothing is linked in a listing, literal, passthrough or comment block,
# in inline code or a passthrough, in a URL or link that is there
# already, or in a cross-reference.  An ADR number with no file is left
# as it is, with a warning.
#
# Load it with "asciidoctor -r ./lib/rose-links.rb" (see the Makefile).

require 'asciidoctor'
require 'asciidoctor/extensions'

module RoseLinks
  GITHUB = 'https://github.com/blancolioni'

  # Other repositories whose issues the book refers to, as NAME#N.
  REPOSITORIES = {
    'idle' => 'idle',
    'syn' => 'syn',
    'raven' => 'raven',
    'rose-os.org' => 'rose-os.org',
  }.freeze

  # The "#" in a link's text, as a character reference, so that
  # Asciidoctor's highlighting, which pairs one "#" with the next, can't
  # take it as the start or end of a highlight.
  HASH = '&#35;'

  # Delimiters of blocks whose text is shown as it is.
  VERBATIM = /\A(-{4,}|\.{4,}|\+{4,}|\/{4,})\s*\z/

  # Parts of a line that are left alone: inline code, passthroughs, URLs
  # (with their link text), link macros and cross-references.
  PROTECTED = %r{
      `[^`]*`
    | \+\+.*?\+\+
    | \+[^+\s][^+]*\+
    | pass:[a-z,]*\[[^\]]*\]
    | (?:https?|link|mailto|xref):[^\s\[]*(?:\[[^\]]*\])?
    | <<[^>]*>>
  }x

  REFERENCE = %r{
      \bADR\s(?<adr>\d{4})\b
    | (?<![\w.\-])(?<repo>#{REPOSITORIES.keys.map { |k| Regexp.escape k }.join '|'})\#(?<repo_number>\d+)\b
    | (?<![\w#&/])\#(?<number>\d{1,5})\b
  }x

  class Linker
    def initialize adr_dir, logger
      @logger = logger
      @adrs = {}
      Dir.glob(File.join(adr_dir, '[0-9][0-9][0-9][0-9]-*.md')).each do |path|
        name = File.basename path
        @adrs[name[0, 4]] = name
      end
    end

    def link_lines lines, path
      verbatim = nil
      lines.map do |line|
        stripped = line.chomp
        if verbatim
          verbatim = nil if stripped == verbatim
          line
        elsif (delimiter = VERBATIM.match(stripped))
          verbatim = delimiter[1]
          line
        elsif stripped.start_with?('//') || stripped.start_with?(':') ||
              stripped.start_with?('[') || stripped.start_with?('include::')
          line
        else
          link_line line, path
        end
      end
    end

    def link_line line, path
      result = +''
      rest = line
      while (protected = PROTECTED.match rest)
        result << link_text(protected.pre_match, path) << protected[0]
        rest = protected.post_match
      end
      result << link_text(rest, path)
    end

    def link_text text, path
      text.gsub(REFERENCE) do
        match = Regexp.last_match
        if match[:adr]
          adr_link match[0], match[:adr], path
        elsif match[:repo]
          %(#{GITHUB}/#{REPOSITORIES[match[:repo]]}/issues/#{match[:repo_number]}) +
            %([#{match[:repo]}#{HASH}#{match[:repo_number]}])
        else
          %(#{GITHUB}/rose/issues/#{match[:number]}[#{HASH}#{match[:number]}])
        end
      end
    end

    def adr_link text, number, path
      if (name = @adrs[number])
        %(#{GITHUB}/rose/blob/main/doc/adr/#{name}[#{text}])
      else
        @logger.warn %(#{File.basename path}: no file for ADR #{number})
        text
      end
    end
  end

  class IncludeChapter < Asciidoctor::Extensions::IncludeProcessor
    def handles? target
      target.end_with? '.adoc'
    end

    def process doc, reader, target, attributes
      path = File.expand_path target, reader.dir
      @linker ||= Linker.new File.expand_path("../adr", doc.base_dir), Asciidoctor::LoggerManager.logger
      lines = File.readlines path, mode: 'r:UTF-8'
      reader.push_include @linker.link_lines(lines, path), path, target, 1, attributes
    end
  end
end

Asciidoctor::Extensions.register do
  include_processor RoseLinks::IncludeChapter
end
