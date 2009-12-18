#
# Copyright (c) 2005-2008, REvolution Computing, Inc.
#
# NetWorkSpaces is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
# USA
#

"""Python API for performing NetWorkSpace operations.

NetWorkSpaces (NWS) is a powerful, open-source software package that
makes it easy to use clusters from within scripting languages like
Python, R, and Matlab.  It uses a Space-based approach, similar to
JavaSpaces (TM) for example, that makes it easier to write distributed
applications.

Example:

First start up the NWS server, using the twistd command:

    % twistd -noy /etc/nws.tac

Now you can perform operations against it using this module:

    % python
    >>> from nws.client import NetWorkSpace
    >>> nws = NetWorkSpace("test")
    >>> nws.store("answer", 42)
    >>> count = nws.fetch("answer")
    >>> print "The answer is", count

"""

import cPickle, os, socket, stat
try:
    import fcntl
except ImportError:
    fcntl = None
try:
    import errno
except ImportError:
    errno = None

from types import StringType

__all__ = ['NwsServer', 'NetWorkSpace', 'NwsException', 'NwsServerException',
           'NwsConnectException', 'NwsOperationException',
           'NwsNoWorkSpaceException', 'NwsDeclarationFailedException',
           'FIFO', 'LIFO', 'MULTI', 'SINGLE',
           'STRING', 'DICT',
           'WS_NAME', 'WS_MINE', 'WS_OWNER', 'WS_PERSISTENT', 'WS_NUMVARS', 'WS_VARLIST',
           'V_VARIABLE', 'V_VALUES', 'V_FETCHERS', 'V_FINDERS', 'V_MODE']

_PythonFP =     0x01000000
_DirectString = 0x00000001

FIFO = 'fifo'
LIFO = 'lifo'
MULTI = 'multi'
SINGLE = 'single'

STRING = 'string'
DICT = 'dict'

WS_NAME = 0
WS_MINE = 1
WS_OWNER = 2
WS_PERSISTENT = 3
WS_NUMVARS = 4
WS_VARLIST = 5

V_VARIABLE = 0
V_VALUES = 1
V_FETCHERS = 2
V_FINDERS = 3
V_MODE = 4

_modes = [FIFO, LIFO, MULTI, SINGLE]
_formats = [STRING, DICT]

class NwsException(Exception):
    """Base class for all exceptions raised by this module."""

class NwsServerException(NwsException):
    """Error communicating with the NWS server."""

class NwsConnectException(NwsServerException):
    """Unable to connect to the NWS server."""

class NwsOperationException(NwsException):
    """Error performing an NWS operation."""

class NwsNoWorkSpaceException(NwsOperationException):
    """No such workspace."""

class NwsDeclarationFailedException(NwsOperationException):
    """Variable declaration failed."""

class NwsServer:

    """Perform operations against the NWS server.

    Operations against workspaces are performed
    by using the NetWorkSpace class.
    """

    def __init__(self, host='localhost', port=8765):
        """Create a connection to the NWS server.

        This constructor is only intended to be called internally.

        Arguments:

        host -- Host name of the NWS server.

        port -- Port of the NWS server.

        """
        self.serverHost = host
        self.serverPort = port
        self.nwsSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        try:
            self.nwsSocket.connect((host, port))
        except socket.error:
            raise NwsConnectException, 'unable to connect to server at %s:%d' % (host, port)

        # denagle the socket to improve performance
        self.nwsSocket.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)

        # enable the keepalive option
        self.nwsSocket.setsockopt(socket.SOL_SOCKET, socket.SO_KEEPALIVE, 1)

        # try to set the 'close on exec' flag for the socket
        if fcntl and hasattr(fcntl, 'FD_CLOEXEC'):
            fd = self.nwsSocket.fileno()
            f = fcntl.fcntl(fd, fcntl.F_GETFD) | fcntl.FD_CLOEXEC
            fcntl.fcntl(fd, fcntl.F_SETFD, f)

        # handshaking that does nothing at the moment.
        self._sendAll('1111')
        self._recvN(4)

    def __str__(self):
        return "NwsServer %s:%d" % (self.serverHost, self.serverPort)

    def _recvN(self, n):
        b = ''
        while len(b) < n:
            d = self.nwsSocket.recv(n-len(b))
            if not d: raise NwsServerException, 'connection dropped?'
            b += d
        return b

    def _sendAll(self, b):
        n = len(b)
        i = 0
        while i < n:
            try:
                m = self.nwsSocket.send(b[i:])
                assert m > 0
                i += m
                if i < n:
                    print >> sys.stderr, 'warning: partial send occurred'
            except socket.error, e:
                if hasattr(e, 'args') and type(e.args) == type(()):
                    if hasattr(errno, 'ETIMEDOUT') and e.args[0] == errno.ETIMEDOUT:
                        print >> sys.stderr, \
                                'warning: got a ETIMEDOUT error while doing a socket send'
                    elif hasattr(errno, 'EAGAIN') and e.args[0] == errno.EAGAIN:
                        print >> sys.stderr, \
                                'warning: got a EAGAIN error while doing a socket send'
                    elif hasattr(errno, 'ENOBUFS') and e.args[0] == errno.ENOBUFS:
                        print >> sys.stderr, \
                                'warning: got a ENOBUFS error while doing a socket send'
                    else:
                        raise
                else:
                    raise

    def close(self):
        """Close the connection to the NWS server.

        s.close()

        This will indirectly cause the NWS server to purge all
        non-persistent workspaces owned by this client.  Purging may
        not happen immediately, though, but will depend on the load on
        the server.

        """
        try:
            self.nwsSocket.shutdown(2)
            self.nwsSocket.close()
        except: pass

    def deleteWs(self, wsName):
        """Delete the specfied workspace on the NWS server.

        s.deleteWs(wsName)

        Arguments:

        wsName -- Name of the workspace to delete.

        """
        if type(wsName) != str:
            raise TypeError, 'workspace name must be a string'

        op = 'delete ws'

        self._sendAll('0002%020d%s%020d%s' % (len(op), op, len(wsName), wsName))

        status = self._recvN(4) # unused at the moment.

    def listWss(self, format=STRING):
        """Return a listing of all of the workspaces on the NWS server.

        s.listWss([format]) -> string or dictionary

        The listing is a string, consisting of lines, each ending with a
        newline.  Each line consists of tab separated fields.  The first
        field is the workspace name, prefixed with either a '>' or a
        space, indicating whether the client owns that workspace or
        not.

        """
        if type(format) != str:
            raise TypeError, 'format must be a string'

        if format not in _formats:
            raise ValueError, 'illegal format: ' + format

        op = 'list wss'
        self._sendAll('0001%020d%s' % (len(op), op))
        status = self._recvN(4) # unused at the moment.
        desc = self._recvN(20) # unused at the moment.
        n = int(self._recvN(20))
        listing = self._recvN(n)

        if format == DICT:
            wss = {}
            if listing:
                for ws in listing[:-1].split('\n'):
                    wsRec = ws.split('\t')
                    mine = wsRec[0][0] == '>'
                    wsRec[WS_NAME] = wsRec[WS_NAME][1:]
                    wsRec.insert(WS_MINE, mine)
                    wsRec[WS_PERSISTENT] = wsRec[WS_PERSISTENT].lower().startswith('t')
                    wsRec[WS_NUMVARS] = int(wsRec[WS_NUMVARS])
                    wsRec[WS_VARLIST] = wsRec[WS_VARLIST] and wsRec[WS_VARLIST].split(',') or []
                    wss[wsRec[WS_NAME]] = tuple(wsRec)
        else:
            wss = listing

        return wss

    def mktempWs(self, wsName='__pyws__%d'):
        """Make a temporary workspace on the NWS server.

        s.mktempWs([wsName]) -> string

        The workspace is named using the template and a number
        generated by the server that makes the workspace name unique.

        Note that the workspace will be created, but it will not be
        owned until some client that is willing to take ownership of it
        opens it.

        The return value is the actual name of workspace that was
        created.

        Arguments:

        wsName -- Template for the workspace name.  This must be a legal
                'format' string, containing only an integer format
                specifier.  The default is '__pyws__%d'.

        Examples:

            Let's create an NwsServer, call mktempWs to make a
            workspace, and then use openWs to create a NetWorkSpace
            object for that workspace:

                >>> from nws.client import NwsServer
                >>> server = NwsServer()
                >>> name = server.mktempWs('example_%d')
                >>> workspace = server.openWs(name)

        """
        if type(wsName) != str:
            raise TypeError, 'workspace name must be a string'

        op = 'mktemp ws'
        self._sendAll('0002%020d%s%020d%s' % (len(op), op, len(wsName), wsName))
        status = int(self._recvN(4))
        desc = self._recvN(20) # unused at the moment.
        name = self._recvN(int(self._recvN(20)))
        if status: raise NwsOperationException, 'mktempWs failed'
        return name

    def openWs(self, wsName, space=None, **opt):
        """Open a workspace.

        s.openWs(wsName[, space]) -> space

        If called without a space argument, this method will construct a
        NetWorkSpace object that will be associated with this NwsServer
        object, and then perform an open operation with it against the NWS
        server.  The open operation tells the NWS server that this
        client wants to use that workspace, and is willing to take
        ownership of it, if it doesn't already exist.

        The space argument is only intended to be used from the
        NetWorkSpace constructor.

        The return value is the constructed NetWorkSpace object.

        Arguments:

        wsName -- Name of the workspace to open.  If the space argument
                is not None, this value must match the space's name.

        space -- NetWorkSpace object to use for the open operation.
                If the value is None, then openWs will construct a
                NetWorkSpace object, specifying this NwsServer object as
                the space's server.  Note that this argument is only
                intended to be used from the NetWorkSpace constructor.
                The default value is None.

        Keyword Arguments:

        persistent -- Boolean value indicating whether the workspace
                should be persistent or not.  See the description of the
                persistent argument in the __init__ method of the
                NetWorkSpace class for more information.

        create -- Boolean value indicating whether the workspace should
                be created if it doesn't already exist.  The default
                value is true.

        Examples:

            Let's create an NwsServer, and then use openWs to create an
            NetWorkSpace object for a workspace called 'foo':

                >>> from nws.client import NwsServer
                >>> server = NwsServer()
                >>> workspace = server.openWs('foo')

            Note that this is (nearly) equivalent to:

                >>> from nws.client import NetWorkSpace
                >>> workspace = NetWorkSpace('foo')

        """
        if type(wsName) != str:
            raise TypeError, 'workspace name must be a string'

        # if invoked directly by user, we need to create a space
        # instance. if invoked via NetWorkSpace constructor, use the
        # space passed in.
        if not space:
            space = NetWorkSpace(wsName, server=self)
        elif space.currentWs() != wsName:
            raise ValueError, 'name of the specified workspace is incorrect'

        op = 'open ws'
        owner = '%d' % os.getpid()

        p = 'no'
        if opt.get('persistent', False): p = 'yes'

        if opt.get('create', True):
            self._sendAll('0004%020d%s%020d%s%020d%s%020d%s' % \
                    (len(op), op, len(wsName), wsName, len(owner), owner,
                        len(p), p))
        else:
            create = 'no'
            self._sendAll('0005%020d%s%020d%s%020d%s%020d%s%020d%s' % \
                    (len(op), op, len(wsName), wsName, len(owner), owner,
                        len(p), p, len(create), create))

        status = int(self._recvN(4))
        if status: raise NwsNoWorkSpaceException, "workspace %s doesn't exist" % repr(wsName)
        return space

    def useWs(self, wsName, space=None, **opt):
        """Use a NetWorkSpace object.

        s.useWs(wsName[, space]) -> space

        If called without a space argument, this method will construct a
        NetWorkSpace object that will be associated with this NwsServer
        object, and then perform a use operation with it against the NWS
        server.  The use operation tells the NWS server that this client
        wants to use that workspace, but is not willing to take
        ownership of it.

        The space argument is only intended to be used from the
        NetWorkSpace constructor.

        The return value is the constructed NetWorkSpace object.

        Arguments:

        wsName -- Name of the workspace to use.  If the space argument
                is not None, this value must match the space's name.

        space -- NetWorkSpace object to use for the use operation.
                If the value is None, then useWs will construct a
                NetWorkSpace object, specifying this NwsServer object as
                the space's server.  Note that this argument is only
                intended to be used from the NetWorkSpace constructor.
                The default value is None.

        Keyword Arguments:

        create -- Boolean value indicating whether the workspace should
                be created if it doesn't already exist.  The default
                value is true.

        Examples:

            Let's create an NwsServer, and then use useWs to create an
            NetWorkSpace object for a workspace called 'foo':

                >>> from nws.client import NwsServer
                >>> server = NwsServer()
                >>> workspace = server.useWs('foo')

            Note that this is (nearly) equivalent to:

                >>> from nws.client import NetWorkSpace
                >>> workspace = NetWorkSpace('foo', useUse=True)

        """
        if type(wsName) != str:
            raise TypeError, 'workspace name must be a string'

        # if invoked directly by user, we need to create a space
        # instance. if invoked via networkspace constructor, use the
        # space passed in.
        if not space:
            space = NetWorkSpace(wsName, server=self)
        elif space.currentWs() != wsName:
            raise ValueError, 'name of the specified workspace is incorrect'

        op = 'use ws'
        owner = ''

        p = 'no'

        if opt.get('create', True):
            self._sendAll('0004%020d%s%020d%s%020d%s%020d%s' % \
                    (len(op), op, len(wsName), wsName, len(owner), owner,
                        len(p), p))
        else:
            create = 'no'
            self._sendAll('0005%020d%s%020d%s%020d%s%020d%s%020d%s' % \
                    (len(op), op, len(wsName), wsName, len(owner), owner,
                        len(p), p, len(create), create))

        status = int(self._recvN(4))
        if status: raise NwsNoWorkSpaceException, "workspace %s doesn't exist" % repr(wsName)
        return space

class NetWorkSpace:

    """Perform operations against workspaces on NWS servers.

    The NetWorkSpace object is the basic object used to perform
    operatations on workspaces.  Variables can be declared, created,
    deleted, and the values of those variables can be manipulated.  You
    can think of a workspace as a network accessible python dictionary,
    where the variable names are keys in the dictionary, and the
    associated values are lists of pickled python objects.  The store
    method puts a value into the list associated with the specified
    variable.  The find method returns a single value from a list.
    Which value it returns depends on the "mode" of the variable (see
    the declare method for more information on the variable mode).  If
    the list is empty, the find method will not return until a value is
    stored in that list.  The findTry method works like the find method,
    but doesn't wait, returning a default value instead (somewhat like
    the dictionary's get method).  The fetch method works like the find
    method, but it will also remove the value from the list.  If
    multiple clients are all blocked on a fetch operation, and a value
    is stored into that variable, the server guarantees that only one
    client will be able to fetch that value.  The fetchTry method, not
    surprisingly, works like the fetch method, but doesn't wait,
    returning a default value instead.
    """

    def __init__(self, wsName='__default', serverHost='localhost', serverPort=8765, useUse=False, server=None, **opt):
        """Construct a NetWorkSpace object for the specified NwsServer.

        Arguments:

        wsName -- Name of the workspace.  There can only be one
                workspace on the server with a given name, so two
                clients can easily communicate with each other by both
                creating a NetWorkSpace object with the same name on the
                same server.  The first client that creates a workspace
                that is willing to take ownership of it, will become the
                owner (see the description of the useUse argument below
                for more information on workspace ownership).

        serverHost -- Host name of the NWS server.  This argument is
                ignored if the server argument is not None.  The default
                value is 'localhost'.

        serverPort -- Port of the NWS server.  This argument is ignored
                if the server argument is not None.  The default value
                is 8765.

        useUse -- Boolean value indicating whether you only want to use
                this workspace, or if you want to open it (which means
                you're willing to take ownership of it, if it's not
                already owned).  

                The default value is False, meaning you are willing to
                take ownership of this workspace.

        server -- NwsServer object to associate with this object.  If
                the value is None (the default value), then a NwsServer
                object will be constructed, using the host and port
                specified with the serverHost and serverPort arguments.

                The default value is None.

        Keyword Arguments:

        persistent -- Boolean value indicating whether the workspace
                should be persistent or not.  If a workspace is
                persistent, it won't be purged when the owner
                disconnects from the NWS server.  Note that only the
                client who actually takes ownership of the workspace
                can make the workspace persistent.  The persistent
                argument is effectively ignored if useUse is True, since
                that client never becomes the owner of the workspace.
                If useUse is False, it is the client who actually
                becomes the owner that determines whether it is
                persistent or not.  That is, after the workspace is
                owned, the persistent argument is ignored.  The default
                value is false.

        create -- Boolean value indicating whether the workspace should
                be created if it doesn't already exist.  The default
                value is true.

        """
        if type(wsName) != str:
            raise TypeError, 'workspace name must be a string'

        self.curWs = wsName

        # if invoked (indirectly) via a NwsServer openWs or useWs method,
        # the server will be passed in and used. if invoked directly,
        # need to create a new NwsServer instance.
        if not server:
            self.server = NwsServer(serverHost, serverPort)
            # now give the server a chance to do its thing.
            try:
                if useUse:
                    self.server.useWs(wsName, self, **opt)
                else:
                    self.server.openWs(wsName, self, **opt)
            except Exception, e:
                # close the server and re-raise the exception
                try: self.server.close()
                except: pass
                raise e
        else:
            self.server = server

        self.send = self.server._sendAll
        self.recv = self.server._recvN

    def __str__(self):
        return "NetWorkspace '%s' [%s]" % (self.curWs, str(self.server))

    def currentWs(self):
        """Return the name of the current workspace.

        ws.currentWs() -> string

        """
        return self.curWs

    def declare(self, varName, mode):
        """Declare a variable in a workspace with the specified mode.

        ws.declare(varName, mode)

        This method is used to specify a mode other than the default
        mode of 'fifo'.  Legal values for the mode are:

            'fifo', 'lifo', 'multi', and 'single'

        In the first three modes, multiple value can be stored in
        a variable.  If the mode is 'fifo', then values are retrieved
        in a "first-in, first-out" fashion.  That is, the first value
        stored, will be the first value fetched.  If the mode is 'lifo',
        then values are retreived in a "last-in, first-out" fashion,
        as in a stack.  If the mode is 'multi', then the order of
        retreival is undefined.

        The 'single' mode means that only a single value can be
        stored in the variable.  Each new store operation will overwrite
        the current value of the variable.

        If a variable is created using a store operation, then the
        mode defaults to 'fifo'.  The mode cannot be changed with
        subsequent calls to declare, regardless of whether the variable
        was originally created using store or declare.

        Arguments:

        varName -- Name of the variable to declare.

        mode -- Mode of the variable.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        if type(mode) != str:
            raise TypeError, 'mode must be a string'

        if mode not in _modes:
            raise ValueError, 'illegal mode: ' + str(mode)

        op = 'declare var'
        self.send('0004%020d%s%020d%s%020d%s%020d%s' % (len(op), op, len(self.curWs), self.curWs, len(varName), varName, len(mode), mode))

        status = int(self.recv(4))
        if status: raise NwsDeclarationFailedException, 'variable declaration failed'

    def deleteVar(self, varName):
        """Delete a variable from a workspace.

        ws.deleteVar(varName)

        All values of the variable are destroyed, and all currently
        blocking fetch and find operations will be aborted.

        Arguments:

        varName -- Name of the variable to delete.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        op = 'delete var'

        self.send('0003%020d%s%020d%s%020d%s' % (len(op), op, len(self.curWs), self.curWs, len(varName), varName))

        status = self.recv(4) # unused at the moment.

    def __retrieve(self, varName, op, missing):
        self.send('0003%020d%s%020d%s%020d%s' % (len(op), op, len(self.curWs), self.curWs, len(varName), varName))
        status = int(self.recv(4)) # barely used at the moment.

        # even if failure status, read the rest of the bytes.
        desc = int(self.recv(20))

        pVal = self.recv(int(self.recv(20)))

        if status: raise NwsOperationException, 'retrieval failed'

        if desc & _DirectString:
            return pVal
        elif pVal:
            return cPickle.loads(pVal)
        else:
            return missing

    def fetch(self, varName):
        """Return and remove a value of a variable from a workspace.

        ws.fetch(varName) -> object

        If the variable has no values, the operation will not return
        until it does.  In other words, this is a "blocking" operation.
        fetchTry is the "non-blocking" version of this method.

        Note that if many clients are all trying to fetch from the same
        variable, only one client can fetch a given value.  Other
        clients may have previously seen that value using the find or
        findTry method, but only one client can ever fetch or fetchTry a
        given value.

        Arguments:

        varName -- Name of the variable to fetch.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        return self.__retrieve(varName, 'fetch', None)

    def fetchTry(self, varName, missing=None):
        """Try to return and remove a value of a variable from a workspace.

        ws.fetchTry(varName[, missing]) -> object

        If the variable has no values, the operation will return
        the value specified by "missing", which defaults to None.

        Note that if many clients are all trying to fetchTry from the
        same variable, only one client can fetchTry a given value.
        Other clients may have previously seen that value using the find
        or findTry method, but only one client can ever fetch or
        fetchTry a given value.

        Arguments:

        varName -- Name of the variable to fetch.

        missing -- Value to return if the variable has no values.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        try:    return self.__retrieve(varName, 'fetchTry', missing)
        except: return missing

    def find(self, varName):
        """Return a value of a variable from a workspace.

        ws.find(varName) -> object

        If the variable has no values, the operation will not return
        until it does.  In other words, this is a "blocking" operation.
        findTry is the "non-blocking" version of this method.

        Note that (unlike fetch) find does not remove the value.  The
        value remains in the variable.

        Arguments:

        varName -- Name of the variable to find.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        return self.__retrieve(varName, 'find', None)

    def findTry(self, varName, missing=None):
        """Try to return a value of a variable in the workspace.

        ws.findTry(varName[, missing]) -> object

        If the variable has no values, the operation will return
        the value specified by "missing", which defaults to the value
        "None".

        Note that (unlike fetchTry) findTry does not remove the value.
        The value remains in the variable.

        Arguments:

        varName -- Name of the variable to use.

        missing -- Value to return if the variable has no values.  The
                default is None.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        try:    return self.__retrieve(varName, 'findTry', missing)
        except: return missing

    def listVars(self, wsName=None, format=STRING):
        """Return a listing of the variables in the workspace.

        ws.listVars([wsName[, format]]) -> string or dictionary

        Arguments:

        wsName -- Name of the workspace to list.  The default is
                None, which means to use the current workspace.

        format -- Output format to return.  Legal values include
                'string' and 'dict'.  The 'string' format returns
                a string which is suitable for printing.
                The 'dict' format returns a dictionary of tuples, where
                the first field is the variable name, the second is
                the number of values, the third is the number of
                fetchers, the fourth is the number of finders, and
                the fifth is the variables mode.  The default value
                is 'string'.

        """
        if wsName is not None and type(wsName) != str:
            raise TypeError, 'workspace name must be a string'

        if type(format) != str:
            raise TypeError, 'format must be a string'

        if format not in _formats:
            raise ValueError, 'illegal format: ' + format

        op = 'list vars'
        if not wsName: wsName = self.curWs
        self.send('0002%020d%s%020d%s' % (len(op), op, len(wsName), wsName))
        status = self.recv(4) # unused at the moment.
        desc = self.recv(20) # unused at the moment.
        n = int(self.recv(20))
        listing = self.recv(n)

        if format == DICT:
            vars = {}
            if listing:
                for var in listing.split('\n'):
                    varRec = var.split('\t')
                    varRec[V_VALUES] = int(varRec[V_VALUES])
                    varRec[V_FETCHERS] = int(varRec[V_FETCHERS])
                    varRec[V_FINDERS] = int(varRec[V_FINDERS])
                    vars[varRec[V_VARIABLE]] = tuple(varRec)
        else:
            vars = listing

        return vars

    def store(self, varName, val):
        """Store a new value into a variable in the workspace.

        ws.store(varName, val)

        Arguments:

        varName -- Name of the variable.

        val -- Value to store in the variable.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        op = 'store'

        desc = _PythonFP
        if StringType == type(val): desc |= _DirectString
        descTxt = '%020u' % desc

        if desc & _DirectString:
            pVal = val
        else:
            pVal = cPickle.dumps(val)

        self.send('0005%020d%s%020d%s%020d%s%020d%s%020d' % (len(op), op, len(self.curWs), self.curWs, len(varName), varName, len(descTxt), descTxt, len(pVal)))
        self.send(pVal)

        status = int(self.recv(4))
        if status: raise NwsOperationException, 'store failed'

    def __retrieveFile(self, varName, op, fobj):
        try:
            fobj.mode.index('b')
        except:
            raise TypeError, 'fobj must be a binary mode file object'

        self.send('0003%020d%s%020d%s%020d%s' % (len(op), op, len(self.curWs), self.curWs, len(varName), varName))
        status = int(self.recv(4)) # barely used at the moment.

        # even if failure status, read the rest of the bytes.
        desc = int(self.recv(20))

        blen = 16 * 1024
        n = int(self.recv(20))
        while n > 0:
            d = self.recv(min(n, blen))
            if not d: raise NwsConnectionException, 'connection dropped?'
            fobj.write(d)
            n -= len(d)

        if status: raise NwsOperationException, 'retrieval failed'
        return True  # XXX what should I return?

    def fetchFile(self, varName, fobj):
        """Return and remove a value of a variable from a workspace.

        ws.fetchFile(varName, fobj)

        Arguments:

        varName -- Name of the variable to fetch.

        fobj -- File to write data to.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        return self.__retrieveFile(varName, 'fetch', fobj)

    def fetchTryFile(self, varName, fobj):
        """Try to return and remove a value of a variable from a workspace.

        ws.fetchTryFile(varName, fobj)

        Arguments:

        varName -- Name of the variable to fetch.

        fobj -- File to write data to.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        try:
            return self.__retrieveFile(varName, 'fetchTry', fobj)
        except:
            return False

    def findFile(self, varName, fobj):
        """Return a value of a variable from a workspace.

        ws.findFile(varName, fobj)

        Arguments:

        varName -- Name of the variable to find.

        fobj -- File to write data to.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        return self.__retrieveFile(varName, 'find', fobj)

    def findTryFile(self, varName, fobj):
        """Try to return a value of a variable in the workspace.

        ws.findTryFile(varName, fobj)

        Arguments:

        varName -- Name of the variable to use.

        fobj -- File to write data to.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        try:
            return self.__retrieveFile(varName, 'findTry', fobj)
        except:
            return False

    def storeFile(self, varName, fobj, n=-1):
        """Store a new value into a variable in the workspace from a file.

        ws.storeFile(varName, fobj[, n])

        Arguments:

        varName -- Name of the variable.

        fobj -- File to read data from to store in the variable.

        n -- Number of bytes to write.  A negative value means to write
                all the data in the file.

        """
        if type(varName) != str:
            raise TypeError, 'variable name must be a string'

        try:
            fobj.mode.index('b')
        except:
            raise TypeError, 'fobj must be a binary mode file object'

        op = 'store'

        desc = _PythonFP | _DirectString
        descTxt = '%020u' % desc

        if n < 0:
            # determine the length of the file using stat
            # if length not specified
            # XXX this looks like it won't work on Windows
            # XXX might need to be opened in binary mode on Windows
            n = os.fstat(fobj.fileno())[stat.ST_SIZE] - fobj.tell()

        self.send('0005%020d%s%020d%s%020d%s%020d%s%020d' % (len(op), op, len(self.curWs), self.curWs, len(varName), varName, len(descTxt), descTxt, n))

        blen = 16 * 1024
        while n > 0:
            d = fobj.read(min(blen, n))
            dlen = len(d)
            if dlen <= 0:
                break
            self.send(d)
            n -= dlen

        if n > 0:
            # warning: they asked us to send more data than they had
            blen = 1024
            buffer = blen * "\0"
            while n > 0:
                if blen <= n:
                    self.send(buffer)
                    n -= dlen
                else:
                    self.send(n * "\0")
                    break

        status = int(self.recv(4))
        if status: raise NwsOperationException, 'store file failed'
