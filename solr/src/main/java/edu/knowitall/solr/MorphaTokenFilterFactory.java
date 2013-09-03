package edu.knowitall.solr;

import java.util.Map;
import org.apache.lucene.analysis.util.TokenFilterFactory;
import org.apache.lucene.analysis.TokenStream;
import org.apache.lucene.analysis.TokenFilter;

public class MorphaTokenFilterFactory extends TokenFilterFactory {

	public MorphaTokenFilterFactory(Map<String, String> args) {
		super(args);
	}

	@Override
	public TokenFilter create(TokenStream input) {
		return new MorphaTokenFilter(input);
	}
}