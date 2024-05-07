require 'sanitize'
require 'loofah'
require 'pg'
require 'getoptlong'
require 'nokogiri'

def fix(s)
  s.gsub("\\", "\\\\").gsub("\"", "\\\"")
end
class Nokogiri::XML::Comment

  def format(*a)
    "(#comment \"" << fix(self.content) <<  "\")"
  end
end
class Nokogiri::XML::Attr

  def format(*a)
    '(#attr "%s" "%s")' % [fix(self.name), fix(self.value)]
  end
end

class Nokogiri::XML::Text

  def format(*a)
    "(#text \"" << fix(self.content) <<  "\")"
  end
end

class Nokogiri::XML::Element
  def format(*a)
    attrs = []
    self.attributes.each do |param|
      attrs << param[1].format
    end

    output = [
      "(#tag \"#{fix(self.name)}\" [",
        #puts param
        attrs.join(', '),
      '] [' ].join(' ')
    r = self.children.map{ |n| n.format }.join(', ')
    output << r << "])"
    output
  end
end

class Loofah::HTML::DocumentFragment
  def format(*a)
    out = "(#document-fragment ["
    cs = []
    self.children.each do |d|
      cs << d.format
    end
    out << cs.join(', ') << "])"
    out
  end
end

class Nokogiri::HTML5::DocumentFragment
  def format(*a)
  out = "(#document-fragment ["
  cs = []
  self.children.each do |d|
    cs << d.format
  end
  out << cs.join(', ') << "])"
  out
  end
end

SanitizerResult = Struct.new(:output, :serialized)

class Fragment
  def gen_id
    @gen_id
  end
  def payload
    @payload
  end
  def tsid
    @tsid
  end

  def print
    puts "#{@gen_id}: #{@payload}"
  end

  def initialize(gen_id, payload, tsid)
    @gen_id = gen_id
    @payload = payload
    @tsid = tsid
  end
end


class Sanitizer
  def name
      @name
  end

  def sanitize(input)
    input
  end

  def initialize()
  end
end

class LoofahStrip < Sanitizer

  def name
    @name = "loofah-strip"
  end

  def sanitize(input)
    fragment = Loofah.fragment(input)
    serialized = fragment.format
    sanitized = fragment.scrub!(:strip).to_s
    SanitizerResult.new(sanitized, serialized)  end
end

class RubySanitize < Sanitizer

  def name
    @name = "ruby-sanitize"
  end

  def sanitize(input)
    done = false
    serialized = ""
    transformer = lambda do |env|
      return if done
      doc = env[:node]
      serialized =  doc.format
      done = true
    end
    sanitized = Sanitize.fragment(input, Sanitize::Config.merge(Sanitize::Config::BASIC,
                                                                :transformers => transformer))
    SanitizerResult.new(sanitized, serialized)
  end
end

class RubySanitizeLax < Sanitizer

  def name
    @name = "ruby-sanitize-lax"
  end

  def sanitize(input)
    done = false
    serialized = ""
    transformer = lambda do |env|
      return if done
      doc = env[:node]
      serialized =  doc.format
      done = true
    end
    sanitized = Sanitize.fragment(input, Sanitize::Config.merge(Sanitize::Config::RELAXED,
                                                   :transformers => transformer,
                                                   :elements        => Sanitize::Config::BASIC[:elements] + %w[
div span title form dfn header p br a style table td tr colgroup svg foreignobject desc path math mtext mglyph mi mo mn ms annotation-xml noscript select input textarea keygen xmp noembed listing li ul pre var dl dt iframe noframes frameset font option object embed
    ],
                                                   #:remove_contents => true
    ))
    SanitizerResult.new(sanitized, serialized)
  end
end
def get_sanitizer_id(conn, name)
  res = conn.exec_params('SELECT id from sanitizers where name = $1', [name])
  id = res[0]['id']
  puts id
  id
end

def insert_sanitized(conn, gen_id, san_id, serialized, output, done, errored, error_message, tsid)
  conn.exec_params('call insert_sanitized($1, $2, $3, $4,  $5, $6, $7, $8)', [gen_id, san_id, serialized, output, errored ? 1 : 0, error_message, done ? 1 : 0, tsid])
end

def get_bext_batch(conn, id)
  batch = Array.new()
  res = conn.exec_params('select ts.id as tsid, g.id, g.payload from to_sanitize ts join generations g on g.id = ts.gen_id where ts.sanitizer_id = $1 and ts.sanitized_id is null LIMIT 25000', [id])
  res.each do |row|
    f = Fragment.new(row['id'], row['payload'], row['tsid'])
    batch.push f
  end
  batch
end

def main()
  conn = PG.connect(host:'database', port:5432, user:'mxssy', password:'allyourmutationsarebelongtous', dbname: 'mxssy' )

  opts = GetoptLong.new(
      [ '--sanitizer', GetoptLong::OPTIONAL_ARGUMENT ]
  )
  name = "ruby-sanitize-lax"
  opts.each do |opt, arg|
      case opt
            when '--sanitizer'
              if arg == ''
                  name = 'ruby-sanitize-lax'
              else
                  name = arg
              end
      end
  end
  sanitizer_map = {}
  sanitizers = [
    LoofahStrip.new(),
    RubySanitize.new(),
    RubySanitizeLax.new()

  ]
  sanitizers.each do |sanitizer|
    sanitizer_map.store(sanitizer.name, sanitizer)
  end

  sanitizer = sanitizer_map[*name]
  sanitizer_id = get_sanitizer_id(conn, sanitizer.name)
  puts "Sanitizing with #{sanitizer.name}: #{sanitizer_id}"
  while true
    batch =  get_bext_batch(conn, sanitizer_id)
    if batch.empty?
      puts "#{sanitizer.name}: Waiting for new fragments..."
      sleep(600)
    else
      batch.each do |fragment|
        begin
          result = sanitizer.sanitize(fragment.payload)
          errored = false
          error_message = ""
        rescue => ex
          sanitized = ""
          errored = true
          error_message = ex.full_message()
        end
        done = not(result.output.include? "mxss")
        #puts "#{result.output}: #{result.serialized}"
        insert_sanitized(conn, fragment.gen_id, sanitizer_id, result.serialized, result.output, done, errored, error_message, fragment.tsid)
      end
    end
  end
end

main()
