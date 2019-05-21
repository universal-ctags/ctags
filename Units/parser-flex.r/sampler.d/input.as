// https://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/sampler/Sample.html
package 
{
    import flash.sampler.*
    import flash.system.*
    import flash.utils.*
    import flash.display.Sprite
    public class sampleTypes extends Sprite
    {
      var b:Boolean = true
        public function sampleTypes() {
            flash.sampler.startSampling();
            for(var i:int=0;i<10000;i++)
              new Object();

            var cpuSamples:Array=[];
            var newSamples:Array=[];
            var delSamples:Array=[];
            var ids:Array=[]

            var lastTime:Number=0;
            for each(var s:Sample in getSamples()) {
              
              assert(s.time > 0); // positive
              assert(Math.floor(s.time) == s.time, s.time); // integral
              assert(s.time >= lastTime, s.time + ":" + lastTime); // ascending
              assert(s.stack == null || s.stack is Array)
              if(s.stack) {
                assert(s.stack[0] is StackFrame);
                assert(s.stack[0].name is String);
            }
              
              if(s is NewObjectSample) {
                var nos = NewObjectSample(s);
                assert(s.id > 0, s.id);
                assert(s.type is Class, getQualifiedClassName(s.type));
                newSamples.push(s);
                ids[s.id] = "got one";
              } else if(s is DeleteObjectSample) {
                var dos = DeleteObjectSample(s);
                delSamples.push(s);
                assert(ids[dos.id] == "got one");
              } else if(s is Sample)
                cpuSamples.push(s);
              else {
                assert(false);
              }
              lastTime = s.time;
            }

            trace(b)
            trace(newSamples.length > 0)
            trace(cpuSamples.length > 0)
            trace(delSamples.length > 0)

        }

        private function assert(e:Boolean, mess:String=null):void {
          b = e && b;
          if(true && !e) {
            if(mess) trace(mess);
            trace(new Error().getStackTrace());
          }     
        }         
    }
}
