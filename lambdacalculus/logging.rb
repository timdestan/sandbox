# Provides a simple logging interface.
# Author: Tim Destan

class Logger
  DEBUG = 10
  INFO = 11
  WARNING = 12 
  ERROR = 13
  FATAL = 14

  @@threshold = Logger::DEBUG

  def self.set_thresh(lvl)
    @@threshold = lvl
  end

  def self.log(msg, level=Logger::DEBUG)
    $stderr.puts wrap(msg,level) if level >= @@threshold
  end

  def self.wrap(msg, level)
    case level
    when Logger::DEBUG
      " -- debug -- #{msg}"
    when Logger::INFO
      " .. info .. #{msg}"
    when Logger::WARNING
      " -- WARNING -- #{msg}"
    when Logger::ERROR
      " ** ERROR ** #{msg}"
    when Logger::FATAL
      " ** FATAL ** #{msg}"
    else
      raise ArgumentError.new("Unknown logging level.")
    end
  end
end

def log(*args)
  Logger.log(*args)
end

def warn(msg)
  Logger.log(msg, Logger::WARNING)
end

def error(msg)
  Logger.log(msg, Logger::ERROR)
end
