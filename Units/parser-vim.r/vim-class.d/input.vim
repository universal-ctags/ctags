vim9script
# Derrived from
# https://github.com/girishji/scope.vim/blob/271ad6d1b76e04cfacf5b30f80103654a2cfce73/autoload/scope/task.vim

export class AsyncCmd
    var job: job

    def Stop(how: string = '')
        if this.job->job_status() ==# 'run'
            how->empty() ? this.job->job_stop() : this.job->job_stop(how)
        endif
    enddef

    def new(cmd: any, CallbackFn: func(list<string>), env: dict<any> = null_dict)
        # ch_logfile('/tmp/channellog', 'w')
        # ch_log('BuildItemsList call')
        var items = []
        this.Stop('kill')
        if cmd->empty()
            return
        endif
        var start = reltime()
        this.job = job_start(cmd, {
            out_cb: (ch, str) => {
                # out_cb is invoked when channel reads 1 line; if you don't care
                # about intermediate output use close_cb
                items->add(str)
                if start->reltime()->reltimefloat() * 1000 > 100 # update every 100ms
                    CallbackFn(items)
                    start = reltime()
                endif
            },
            close_cb: (ch) =>  CallbackFn(items),
            err_cb: (chan: channel, msg: string) => {
                # ignore errors
                # :echohl ErrorMsg | echoerr $'error: {msg} from {cmd}' | echohl None
            },
        }->extend(env != null_dict ? {env: env} : {}))
    enddef
endclass
