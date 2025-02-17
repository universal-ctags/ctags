const Comp1 = () => {
    const x = (arg) => console.log(arg)
    x(4)
    return(<Comp2 text={<p>Some Text</p>}/>)
}

const Comp2 = (props) => {
    return (
	props.text
    )
}

const Comp3 = (str) => {
    return(<div>
	       <h1>hello</h1>
	       something: {str}
	       <h1>bye</h1>
	   </div>)
}

const Comp4 = (str) => {
    return(<>
	       <h1>bonjour</h1>
	       something: {str}
	       {<h2>frag{subtitle}ment</h2>}
	       <h1>au revoir</h1>
	   </>)
}

const Comp5 = (str) => {
    return <x/>
}

var z0

// Taken from https://stackoverflow.com/questions/79395369/why-does-universal-ctags-fail-to-record-some-functions-in-the-tags-file-for-a-pr
